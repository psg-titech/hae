module Hae.Internal.MergePrimitive where

import Control.Monad.State.Lazy
import Data.Graph.Inductive.Graph (labEdges)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set
import Hae.Internal.FOAS
import Hae.Internal.NGraph as B

import Debug.Trace

-- primitive operators are merged together to form a closure
-- other nodes are called individual nodes
data MergeState = MergeState
  { closures :: Set NodeId
  , individuals :: Set NodeId
  , nodeToClosure :: Map NodeId NodeId
  , argOfFunc :: Map NodeId NodeId
  }

type ClozId = NodeId

type Merge a = State MergeState a

addClosure :: ClozId -> Merge ()
addClosure cid = do
  state <- get
  put state {closures = Set.insert cid (closures state)}

addIndividualNode :: NodeId -> Merge ()
addIndividualNode nid = do
  state <- get
  put state {individuals = Set.insert nid (individuals state)}

markNodeInClosure :: NodeId -> ClozId -> Merge ()
markNodeInClosure nid cid = do
  state <- get
  put state {nodeToClosure = Map.insert nid cid (nodeToClosure state)}

setArgOfFunc :: NodeId -> NodeId -> Merge ()
setArgOfFunc aid fid = do
  state <- get
  let aof = argOfFunc state
  put state {argOfFunc = Map.insert fid aid aof}

emptyMergeState =
  MergeState
    { closures = Set.empty
    , individuals = Set.empty
    , nodeToClosure = Map.empty
    , argOfFunc = Map.empty
    }

mergePrimitive ::
     [NodeId]
  -> Map NodeId Node
  -> B.NGraph
  -> (Set ClozId, Map NodeId ClozId, Map ClozId (Set NodeId), Set NodeId, Map NodeId NodeId)
mergePrimitive outs nodes ngraph =
  (closures ms, ntc, ioc, individuals ms, argOfFunc ms)
  where
    ms = execState (mapM_ (\x -> goNode x x nodes False) outs) emptyMergeState
    ntc = nodeToClosure ms
    ioc =
      Prelude.foldr (step . f2) Map.empty $ Prelude.filter f1 (labEdges ngraph)
      where
        f1 (x, par, _) =
          not
            ((x `Map.member` ntc) &&
             (par `Map.member` ntc) && (ntc ! x /= ntc ! par))
        f2 (x, par, _) = if par `Map.member` ntc then (ntc ! par, x)
                                                 else (par, x)
        step ::
             (NodeId, NodeId)
          -> Map ClozId (Set NodeId)
          -> Map ClozId (Set NodeId)
        step (k, v) = Map.insertWith Set.union k (Set.singleton v)

goNode :: NodeId -> ClozId -> Map NodeId Node -> Bool -> Merge ()
goNode nid cid nodes parentIsPrimitive = do
  ntc <- nodeToClosure <$> get
  if nid `Map.member` ntc
    then unless (ntc ! nid == cid) $ goInnerNode nid nid False
    else goInnerNode nid cid parentIsPrimitive
  where
    goInnerNode nid cid parentIsPrimitive =
      case nodes ! nid of
        FuncN c a -> do
          unless parentIsPrimitive $ addClosure nid
          markNodeInClosure nid cid
          setArgOfFunc a c -- for finding the type of arguments
          goNode a cid nodes True
        Prim1N _ arg -> do
          unless parentIsPrimitive $ addClosure nid
          markNodeInClosure nid cid
          goNode arg cid nodes True
        Prim2N _ arg1 arg2 -> do
          unless parentIsPrimitive $ addClosure nid
          markNodeInClosure nid cid
          goNode arg1 cid nodes True
          goNode arg2 cid nodes True
        IntLN _ -> markNodeInClosure nid cid
        BoolLN _ -> markNodeInClosure nid cid
        FloatLN _ -> markNodeInClosure nid cid
        DoubleLN _ -> markNodeInClosure nid cid
        -- FOAS node types below are not primitive nodes
        SwitchN p t f -> do
          addIndividualNode nid
          goNode p p nodes False
          goNode t t nodes False
          goNode f f nodes False
        SampleN t r -> do
          addIndividualNode nid
          goNode t t nodes False
          goNode r r nodes False
        PrevN p -> do
          addIndividualNode nid
          goNode p p nodes False
        InputN _ _ -> addIndividualNode nid
        PairN p1 p2 -> undefined -- TODO: not implemented yet
