{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Hae.Compiler
  ( OutputDef(..)
  , BuildTarget(..)
  , compile
  , debugNode
  , debugGraph
  ) where

import Control.Monad.State.Lazy
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set
import Data.Typeable
import qualified Hae.Internal.CodeGen as C
import qualified Hae.Internal.Deref as D
import Hae.Internal.Expr
import Hae.Internal.FOAS
import qualified Hae.Internal.MergePrimitive as M
import qualified Hae.Internal.NGraph as B
import Hae.Internal.Util.TypedList
import Hae.Types

import Data.Graph.Inductive.Graph (prettyPrint)
import Debug.Trace

data OutputDef =
  forall t f. HType t =>
              OutputDef String
                        (OE (K NodeId) t)

type Program = String

data BuildTarget = Mbed | Debug

compile :: BuildTarget -> [OutputDef] -> Program
compile bt = (stage4 bt) . stage3 . stage2 . stage1

data Stage1Env = Stage1Env
  { s1OutDef :: [(String, NodeId)]
  , s1InNodes :: [NodeId]
  , s1NodeMap :: Map NodeId Node
  , s1TypeEnv :: Map NodeId TypeTag
  , s1LiftedClosures :: [NodeId]
  } deriving (Show)

-- |Stage1: build FOAS, typing
stage1 :: [OutputDef] -> Stage1Env
stage1 outDefs =
  let analyzeOutputDef (OutputDef name oe) = do
        oid <- D.analyzeOE oe
        return (name, oid)
      (outNodesDesc, derefState) =
        runState (mapM analyzeOutputDef outDefs) D.emptyState
  in Stage1Env
     { s1OutDef = outNodesDesc
     , s1InNodes = D.inputs derefState
     , s1NodeMap = D.nodes derefState
     , s1TypeEnv =
         Map.map (fromMaybe (error "Incomplete typing")) (D.typeEnv derefState)
     , s1LiftedClosures = D.closures derefState
     }

data Stage2Env = Stage2Env
  { s2OutDef :: [(String, NodeId)]
  , s2InTimings :: Map NodeId [B.Timing]
  , s2NodeMap :: Map NodeId Node
  , s2TypeEnv :: Map NodeId TypeTag
  , s2LiftedClosures :: [NodeId]
  , s2NGraph :: B.NGraph
  } deriving (Show)

-- |Stage2: build dependency graph, compute UpdateRate and Timing
stage2 :: Stage1Env -> Stage2Env
stage2 s1env =
  let outs = s1OutDef s1env
      nm = s1NodeMap s1env
      ins = s1InNodes s1env
      (ng, ur) = B.buildGraphUR (Prelude.map snd outs) nm
      tm = B.genTiming ng ur nm
  in Stage2Env
     { s2OutDef = outs
     , s2InTimings = Map.filterWithKey (\k _ -> k `elem` ins) tm
     , s2NodeMap = nm
     , s2TypeEnv = s1TypeEnv s1env
     , s2LiftedClosures = s1LiftedClosures s1env
     , s2NGraph = ng
     }

data Stage3Env = Stage3Env
  { s3OutDef :: [(String, NodeId)]
  , s3InTimings :: Map NodeId [B.Timing]
  , s3NodeMap :: Map NodeId Node -- not merged
  , s3TypeEnv :: Map NodeId TypeTag
  , s3LiftedClosures :: [NodeId]
  , s3Closures :: Set M.ClozId -- merged primitive closures
  , s3NodeToClosure :: Map NodeId M.ClozId -- map inner nodes to primitive closures
  , s3InputOfClosure :: Map M.ClozId [NodeId] -- argument of primitive closures
  , s3PushOrder :: [NodeId]
  , s3FuncArgType :: Map NodeId TypeTag
  }

-- |Stage3: merge primitive functions
stage3 :: Stage2Env -> Stage3Env
stage3 s2env =
  let outs = s2OutDef s2env
      nm = s2NodeMap s2env
      nGraph = s2NGraph s2env
      (cz, ntc, ioc, inds, aof) = M.mergePrimitive (Prelude.map snd outs) nm nGraph
      po = Prelude.filter (\x -> x `elem` (Set.toList inds ++ Set.toList cz)) $ topsort nGraph
  in Stage3Env
     { s3OutDef = outs
     , s3InTimings = s2InTimings s2env
     , s3NodeMap = nm
     , s3TypeEnv = s2TypeEnv s2env
     , s3LiftedClosures = s2LiftedClosures s2env
     , s3Closures = cz
     , s3NodeToClosure = ntc
     , s3InputOfClosure =  Map.map (\ns -> Prelude.filter (`Set.member` ns) po) ioc
     , s3PushOrder = po
     , s3FuncArgType = Map.map (\argNId -> s2TypeEnv s2env ! argNId) aof
     }

-- |Stage4: code generation
stage4 :: BuildTarget -> Stage3Env -> Program
stage4 bt s3env =
  let outs = s3OutDef s3env
      inps = s3InTimings s3env
      nodes = s3NodeMap s3env
      types = s3TypeEnv s3env
      lifted = s3LiftedClosures s3env
      clozs = s3Closures s3env
      ntc = s3NodeToClosure s3env
      ioc = s3InputOfClosure s3env
      pushOrder = s3PushOrder s3env
      argType = s3FuncArgType s3env
      btStr = case bt of
        Mbed -> "mbed"
        Debug -> "debug"
  in C.gen outs inps nodes types lifted argType clozs ntc ioc pushOrder btStr

-----------------------------------------------------------------------------
debugNode :: Typeable t => OE (K Int) t -> IO ()
debugNode n = putStrLn $ D.showNodes $ execState (D.analyzeOE n) D.emptyState

debugGraph n =
  let s2env = stage2 $ stage1 [OutputDef "target" n]
      ng = s2NGraph s2env
  in putStrLn "Graph:" >> prettyPrint ng >> putStrLn "Inp Timing:" >>
     print (s2InTimings s2env)