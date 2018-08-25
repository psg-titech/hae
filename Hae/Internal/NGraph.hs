{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hae.Internal.NGraph where

import Control.Monad.State.Lazy
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Inductive.Graph (LEdge, mkGraph, out)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.List (foldl', sort, intercalate,sort)
import Data.Map.Strict as Map
import Data.Set as Set
import Hae.Internal.Expr
import Hae.Internal.FOAS

------------------------------- Stage 2 -------------------------------
-- push graph and update rate generation

type NGraph = Gr VertexLabel EdgeLabel

type UpdateRate = Int -- in ms

type VertexLabel = ()

data EdgeLabel
  = Normal
  | Past
  deriving (Show)

data BuildState = BuildState
  { visited :: Set NodeId
  , vertexList :: [(Int, VertexLabel)]
  , edgeList :: [(Int, Int, EdgeLabel)]
  , updateRateMap :: Map NodeId UpdateRate
  }

emptyBuildState =
  BuildState
  { visited = Set.empty
  , vertexList = []
  , edgeList = []
  , updateRateMap = Map.empty
  }

type BuildGraph a = State BuildState a

hasVisited :: NodeId -> BuildGraph Bool
hasVisited nid = Set.member nid . visited <$> get

markVisited :: NodeId -> BuildGraph ()
markVisited nid = do
  state <- get
  let vs = visited state
  put state {visited = Set.insert nid vs}

addVertex :: NodeId -> BuildGraph ()
addVertex nid = do
  state <- get
  let vl = vertexList state
  put state {vertexList = (nid, ()) : vl}

addEdge :: NodeId -> NodeId -> EdgeLabel -> BuildGraph ()
addEdge from to label = do
  state <- get
  let el = edgeList state
  put state {edgeList = (from, to, label) : el}

getUpdateRate :: NodeId -> BuildGraph UpdateRate
getUpdateRate nid = do
  state <- get
  return $
    updateRateMap state ! nid

setUpdateRate :: NodeId -> UpdateRate -> BuildGraph ()
setUpdateRate nid ur = do
  state <- get
  let urMap = updateRateMap state
  put state {updateRateMap = Map.insert nid ur urMap}

buildGraphUR :: [NodeId] -> Map NodeId Node -> (NGraph, Map NodeId UpdateRate)
buildGraphUR outNIds nodeMap =
  let bState =
        Prelude.foldr
          (\n prevState -> execState (analyzeFOAS n nodeMap) prevState)
          emptyBuildState
          outNIds
  in (mkGraph (vertexList bState) (edgeList bState), updateRateMap bState)

timingDefToUpdateRate :: TimingDef -> UpdateRate
timingDefToUpdateRate (EveryMs ms) = ms
timingDefToUpdateRate (EveryS s) = s * 1000
timingDefToUpdateRate (EveryMin m) = m * 60 * 1000
timingDefToUpdateRate AsyncInp = 0

analyzeFOAS :: NodeId -> Map NodeId Node -> BuildGraph ()
analyzeFOAS nid nodeMap =
  hasVisited nid >>= \p ->
    if p
      then return ()
      else markVisited nid >>
           let node = nodeMap ! nid
           in case node of
                IntLN _ -> addVertex nid >> setUpdateRate nid 0
                BoolLN _ -> addVertex nid >> setUpdateRate nid 0
                FloatLN _ -> addVertex nid >> setUpdateRate nid 0
                DoubleLN _ -> addVertex nid >> setUpdateRate nid 0
                InputN _ timingDef ->
                  addVertex nid >>
                  setUpdateRate nid (timingDefToUpdateRate timingDef)
                Prim1N _ dep ->
                  addVertex nid >> analyzeFOAS dep nodeMap >>
                  addEdge dep nid Normal >>
                  getUpdateRate dep >>=
                  setUpdateRate nid
                Prim2N _ dep1 dep2 -> do
                  addVertex nid
                  analyzeFOAS dep1 nodeMap
                  analyzeFOAS dep2 nodeMap
                  addEdge dep1 nid Normal
                  addEdge dep2 nid Normal
                  ur1 <- getUpdateRate dep1
                  ur2 <- getUpdateRate dep2
                  setUpdateRate nid $ gcd ur1 ur2
                SwitchN dep1 dep2 dep3 -> do
                  addVertex nid >> analyzeFOAS dep1 nodeMap
                  analyzeFOAS dep2 nodeMap
                  analyzeFOAS dep3 nodeMap
                  addEdge dep1 nid Normal
                  addEdge dep2 nid Normal
                  addEdge dep3 nid Normal
                  ur1 <- getUpdateRate dep1
                  ur2 <- getUpdateRate dep2
                  ur3 <- getUpdateRate dep3
                  setUpdateRate nid (gcd (gcd ur1 ur2) ur3)
                PrevN dep ->
                  addVertex nid >> analyzeFOAS dep nodeMap >>
                  addEdge dep nid Past >>
                  getUpdateRate dep >>=
                  setUpdateRate nid
                SampleN tgt ref -> do
                  addVertex nid
                  analyzeFOAS tgt nodeMap
                  analyzeFOAS ref nodeMap
                  addEdge tgt nid Normal
                  addEdge ref nid Normal
                  refUR <- getUpdateRate ref
                  setUpdateRate nid refUR
                FuncN lam arg -> do
                  addVertex nid
                  analyzeFOAS arg nodeMap
                  addEdge arg nid Normal --FIXME: lam should be pure function, currently Hae doesn't force that
                  ur <- getUpdateRate arg --FIXME:  lam should be pure function, currently Hae doesn't force that
                  setUpdateRate nid ur
                VarN varId -> undefined -- TODO:
                PairN _ _ -> undefined -- TODO:

-- timing generation

newtype GenTimingState = GenTimingState
  { timingMap :: Map NodeId [Timing]
  }

getVertexTiming :: NodeId -> GenTiming [Timing]
getVertexTiming nid = do
  state <- get
  return $ timingMap state ! nid

setVertexTiming :: NodeId -> [Timing] -> GenTiming ()
setVertexTiming nid timing = do
  state <- get
  put state {timingMap = Map.insert nid timing (timingMap state)}

type GenTiming a = State GenTimingState a

data Timing
  = NormalTiming Int
  | PastTiming Int
               (Set Int)
  deriving (Eq, Show)

listPastTimingStr :: Set Int -> String
listPastTimingStr = intercalate ", " . Prelude.map show . sort . Set.toList

isNormalTiming (NormalTiming _) = True
isNormalTiming _ = False

isPastTiming (PastTiming _ _) = True
isPastTiming _ = False

emptyGenTimingState = GenTimingState {timingMap = Map.empty}

genTiming :: NGraph
          -> Map NodeId UpdateRate
          -> Map NodeId Node
          -> Map NodeId [Timing]
genTiming nGraph urMap nodeMap =
  let topOrder = topsort (grev nGraph)
      gState =
        Data.List.foldl'
          (\s n -> execState (analyzeTiming n nGraph urMap) s)
          emptyGenTimingState
          topOrder
  in timingMap gState

analyzeTiming :: NodeId -> NGraph -> Map NodeId UpdateRate -> GenTiming ()
analyzeTiming nid nGraph urMap = do
  let updateRate = urMap ! nid
  case out nGraph nid of
    [] -> setVertexTiming nid [NormalTiming updateRate]
    parents -> do
      timingNotMerged <- go parents -- firstly combine all timings together
      setVertexTiming nid (mergeTiming timingNotMerged) -- then merge them
      where go :: [LEdge EdgeLabel] -> GenTiming [Timing]
            go ((_, par, edgeLabel):ps) =
              case edgeLabel of
                Normal -> do
                  parTiming <- getVertexTiming par
                  psTm <- go ps
                  return $ parTiming ++ psTm
                Past -> do
                  parTiming <- getVertexTiming par
                  psTm <- go ps
                  return $
                    Prelude.map (prevRefOnTiming updateRate) parTiming ++ psTm
            go [] = return []
            prevRefOnTiming :: UpdateRate -> Timing -> Timing
            -- is this too naive to be correct in some corner cases?
            prevRefOnTiming ur (NormalTiming i) =
              PastTiming i (Set.singleton (negate ur))
            prevRefOnTiming ur (PastTiming i s) =
              PastTiming
                i
                (Set.fromList $ Set.toList s >>= (\x -> [x, x - ur])) -- OMG, Data.Set is not a Monad
            mergeTiming :: [Timing] -> [Timing]
            mergeTiming ts =
              mergeNormal (Prelude.filter isNormalTiming ts) ++
              mergePast (Prelude.filter isPastTiming ts)
              where
                mergeNormal ns =
                  Prelude.map NormalTiming $ Prelude.foldr step [] $ sort $ Prelude.map unNT ns
                  where
                    unNT (NormalTiming t) = t
                    step v acc = if any (\a -> v `mod` a == 0) acc then acc
                                                                   else v : acc
                mergePast ps = mapBackToPTL $ Prelude.foldr step Map.empty ps
                  where
                    mapBackToPTL :: Map Int (Set Int) -> [Timing]
                    mapBackToPTL ps = Prelude.foldr (\(p,pts) acc -> PastTiming p pts:acc) [] (Map.toList ps)
                    step :: Timing -> Map Int (Set Int) -> Map Int (Set Int)
                    step (PastTiming t pts) = Map.insertWith Set.union t pts