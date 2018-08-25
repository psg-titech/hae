{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Hae.Internal.Deref where

import Debug.Trace

import Control.Monad (when)
import Control.Monad.State.Lazy
import Data.List (stripPrefix)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Typeable
import Data.Unique
import Hae.Expr
import Hae.Internal.Expr
import Hae.Internal.FOAS
import Hae.Internal.PrimOp (primTyping1, primTyping2)
import Hae.Internal.Util.Ref
import Hae.Internal.Util.TypedList
import Hae.Types

-- Reference Analysis
-- This part transforms beta-reducted expressions into FOAS
--
-- Convert Unique to increasing NodeId for better readability
-- We use NodeId as VarId in FOAS
data DerefState = DerefState
  { nextId :: NodeId
  , inputs :: [NodeId]
  , closures :: [NodeId] -- user lifted functions
  , nodes :: Map NodeId Node -- FOAS nodes
  , typeEnv :: Map NodeId (Maybe TypeTag) -- type of FOAS nodes
  , refMap :: Map Unique NodeId
  }

emptyState =
  DerefState
    { nextId = 0
    , inputs = []
    , closures = []
    , refMap = Map.empty
    , nodes = Map.empty
    , typeEnv = Map.empty
    }

type Deref a = State DerefState a

-- some helper functions for Deref Monad
updateId :: Deref NodeId
updateId = do
  state <- get
  let nid = nextId state
  put state {nextId = nid + 1}
  return nid

markNIdAsClosure :: NodeId -> Deref ()
markNIdAsClosure nid = do
  state <- get
  put state {closures = nid : closures state}

markNIdAsInput :: NodeId -> Deref ()
markNIdAsInput nid = do
  state <- get
  put state {inputs = nid : inputs state}

setNode :: NodeId -> Node -> Deref ()
setNode nid node = do
  state <- get
  put state {nodes = Map.insert nid node (nodes state)}

setNodeIfNonExistent :: NodeId -> Node -> Deref ()
setNodeIfNonExistent nid node = do
  state <- get
  let nodeMap = nodes state
  case Map.lookup nid nodeMap of
    Nothing -> put state {nodes = Map.insert nid node nodeMap}
    _ -> return ()

setNodeType :: NodeId -> TypeTag -> Deref ()
setNodeType nid typeTag = do
  state <- get
  put state {typeEnv = Map.insert nid (Just typeTag) (typeEnv state)}

setTypedNode :: NodeId -> Node -> TypeTag -> Deref ()
setTypedNode nid node typeTag = setNode nid node >> setNodeType nid typeTag

getTypeFromEnv :: NodeId -> Deref (Maybe TypeTag)
getTypeFromEnv nid = do
  state <- get
  case Map.lookup nid (typeEnv state) of
    Just v -> return v
    _ -> return Nothing

getNode :: NodeId -> Deref Node
getNode nid = do
  state <- get
  case Map.lookup nid (nodes state) of
    Just node -> return node
    Nothing -> error $ "Node " ++ show nid ++ " not in map yet"

-- |Return node ID if it has been seen before, otherwise assign a new ID for it
refToNodeId :: Ref a -> Deref NodeId
refToNodeId ref = do
  state <- get
  let id_this = nextId state
  case Map.lookup (refId ref) (refMap state) of
    Just nodeId -> return nodeId
    Nothing ->
      put
        state
          { nextId = id_this + 1
          , refMap = Map.insert (refId ref) id_this (refMap state)
          } >>
      return id_this

-- |Special purpose function only for letrec bindings
setNodeIdForRef :: Ref a -> NodeId -> Deref ()
setNodeIdForRef ref nid = do
  state <- get
  put state {refMap = Map.insert (refId ref) nid (refMap state)}

castValToTypedNode :: Typeable a => a -> Maybe (Node, TypeTag)
castValToTypedNode v =
  msum
    [ fmap (\x -> (IntLN x, TInt)) (cast v :: Maybe Int)
    , fmap (\x -> (BoolLN x, TBool)) (cast v :: Maybe Bool)
    -- TODO: () and other types
    ]

-- TODO: refactor messy code
analyzeType :: NodeId -> Deref (Maybe TypeTag)
analyzeType nid = do
  mbT <- getTypeFromEnv nid
  mbResType <-
    case mbT of
      Just tt -> return $ Just tt
      Nothing ->
        getNode nid >>= \case
          Prim1N op aid -> fmap (primTyping1 op) <$> analyzeType aid
          Prim2N op aid bid -> do
            mbAType <- analyzeType aid
            mbBType <- analyzeType bid
            return $ liftM2 (primTyping2 op) mbAType mbBType
          PairN aid bid -> do
            mbAType <- analyzeType aid
            mbBType <- analyzeType bid
            return $ liftM2 TPair mbAType mbBType
          SwitchN _ tid fid -> do
            mbTType <- analyzeType tid
            mbFType <- analyzeType fid
            if mbTType == mbFType
              then return mbTType
              else error "IfThenElse statement has invalid type"
          VarN vid -> analyzeType vid
      -- FIXME: Lam ...
  -- TODO: refactor
  case mbResType of
    Just resType -> setNodeType nid resType
    Nothing -> return ()
  return mbResType

-- TODO: what about Void??
-- |transform OE to FOAS node and add it to env if not already
analyzeOE :: OE (K NodeId) a -> Deref NodeId
analyzeOE (OE r) = do
  nid <- refToNodeId r
  case deref r of
    Var hole
      -- setNode nid (VarN (unK hole))
      -- return nid
     -> do
      setNodeIfNonExistent nid (VarN (unK hole))
      return $ unK hole
    expr@(Inp inpName timing) -> do
      let inpType = typeRep2TypeTag $ typeOf expr
      setTypedNode nid (InputN inpName timing) inpType
      markNIdAsInput nid
      return nid
    Lit v ->
      case castValToTypedNode v of
        Just (castNode, typeTag) ->
          setTypedNode nid castNode typeTag >> return nid
        Nothing -> error "Cannot cast Val to Node"
    expr@(Pair a b) -> do
      let pairType = typeRep2TypeTag $ typeOf expr
      aid <- analyzeOE a
      bid <- analyzeOE b
      case pairType of
        TPair at bt -> setNodeType aid at >> setNodeType bid bt -- TODO: is it good to do it here?
        _ -> error "impossible"
      setTypedNode nid (PairN aid bid) pairType
      return nid
    expr@(App (OE lamR) arg) -> do
      let appResultType = typeRep2TypeTag $ typeOf expr
      lamRNId <- refToNodeId lamR -- also observe the sharing of lamR
      argNId <- analyzeOE arg
      case deref lamR of
        PrimOp pOpId _ -- lamRNId is not used here; all operations expanded
         ->
          case pOpId of
            PrimOpId1 op -> do
              setTypedNode nid (Prim1N op argNId) appResultType
              return nid
            PrimOpId2 op -> do
              pn <- getNode argNId
              case pn of
                PairN arg1Id arg2Id -> do
                  setTypedNode nid (Prim2N op arg1Id arg2Id) appResultType
                  return nid
                _ -> error "Primitive2 not applied to arg pair"
        expr@(Lam hl) -> do
          lamNId <- analyzeOE (hl (K (-1))) -- FIXME: magic number -1 here
          let newType = typeRep2TypeTag (typeOf expr)
          setTypedNode nid (FuncN lamNId argNId) newType
          -- add lamNId to closures list so that compiler doesn't forget them
          markNIdAsClosure lamNId
          return nid
    IfThenElse p t f -> do
      pNId <- analyzeOE p
      tNId <- analyzeOE t
      fNId <- analyzeOE f
      ifExpType <- getTypeFromEnv tNId
      setTypedNode
        nid
        (SwitchN pNId tNId fNId)
        (fromMaybe (error "IfThenElse type infer error") ifExpType)
      return nid
    expr@(LetRec bindings e) -- generate nid for each binding first, then apply the nidSeqs TList to bindings and e
     -> do -- I wrote this code so long ago that I cannot understand it...
      state <- get
      let nidStart = nextId state
      let nidSeqs = tenumFrom nidStart
      let nidEnd = nidStart + tlength nidSeqs
      put state {nextId = nidEnd}
      varMap <- addBindingsToEnv (bindings nidSeqs) nidSeqs -- add mappedBindings to environment, recursively
      -- repeat typing each binding until every binding is typed
      let varResolvedSeqs = transformSeqs nidSeqs varMap
      typeBindings (ttoList varResolvedSeqs)
      let mappedExpr = e varResolvedSeqs
      -- set mappedExpr's nid to LetRec's nid
      case mappedExpr of
        OE r -> setNodeIdForRef r nid
      analyzeOE mappedExpr -- the return value should be the same as nid
      where addBindingsToEnv ::
                 TList (OE (K NodeId)) ts
              -> TList (K Int) ts'
              -> Deref (Map NodeId NodeId)
            addBindingsToEnv (e ::: es) (nidBinding ::: ys) =
              case e of
                OE r -> do
                  setNodeIdForRef r (unK nidBinding)
                  enid <- analyzeOE e
                  enode <- getNode (unK nidBinding)
                  case enode of
                    VarN _ ->
                      Map.insert (unK nidBinding) enid <$>
                      addBindingsToEnv es ys
                    _ -> addBindingsToEnv es ys
            addBindingsToEnv TNil TNil = return Map.empty
            transformSeqs seqs varMap = tmapK f seqs
              where
                f x =
                  case Map.lookup x varMap of
                    Nothing -> x
                    Just v -> f v
            typeBindings seqs = do
              typingResult <- mapM analyzeType seqs :: Deref [Maybe TypeTag]
              when (any isNothing typingResult) $ typeBindings seqs
    PrevVal target -> do
      targetNId <- analyzeOE target
      mbTargetType <- getTypeFromEnv targetNId
      case mbTargetType of
        Just targetType ->
          setTypedNode nid (PrevN targetNId) targetType >> return nid
        Nothing -> error "IMPOSSIBLE?"
    SampleOnChange target ref -> do
      targetNId <- analyzeOE target
      refNId <- analyzeOE ref
      mbTargetType <- getTypeFromEnv targetNId
      case mbTargetType of
        Just targetType ->
          setTypedNode nid (SampleN targetNId refNId) targetType >> return nid
        Nothing -> error "IMPOSSIBLE2?"

-- DEBUG
showNodes :: DerefState -> String
showNodes s = Map.foldlWithKey' step "" nMap
  where
    step :: String -> NodeId -> Node -> String
    step acc nid n =
      acc ++
      "\n" ++ "n" ++ show nid ++ " = " ++ show n ++ " :: " ++ showType nid
    nMap = nodes s
    showType nid = show $ typeEnv s ! nid
