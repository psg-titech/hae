module Hae.Internal.CodeGen where

import Data.DList as DL
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set
import Hae.Expr
import Hae.Internal.FOAS
import qualified Hae.Internal.MergePrimitive as M
import Hae.Internal.NGraph (NGraph)
import qualified Hae.Internal.NGraph as B
import Text.Printf

-- helper function
typeTag2cppStr TInt = "int"
typeTag2cppStr TDouble = "double"
typeTag2cppStr TBool = "bool"
typeTag2cppStr TFloat = "float"
typeTag2cppStr _ = undefined

genPrimExp :: Map NodeId Node -> NodeId -> String
genPrimExp nodes nid =
  let helper opStr arg1 arg2 =
        "(" ++
        genPrimExp nodes arg1 ++
        ")" ++ opStr ++ "(" ++ genPrimExp nodes arg2 ++ ")"
   in case nodes ! nid of
        Prim1N f arg
          | f == "abs" -> "std::abs(" ++ genPrimExp nodes arg ++ ")"
          | f == "signum" -> "hae::signum(" ++ genPrimExp nodes arg ++ ")"
          | f == "not" -> "!(" ++ genPrimExp nodes arg ++ ")"
        Prim2N f arg1 arg2
          | f == "(+)" -> helper "+" arg1 arg2
          | f == "(-)" -> helper "-" arg1 arg2
          | f == "(*)" -> helper "*" arg1 arg2
          | f == "(/)" -> helper "/" arg1 arg2
          | f == "(>)" -> helper ">" arg1 arg2
          | f == "(<)" -> helper "<" arg1 arg2
          | f == "(==)" -> helper "==" arg1 arg2
          | f == "(/=)" -> helper "!=" arg1 arg2
          | f == "(<=)" -> helper "<=" arg1 arg2
          | f == "(>=)" -> helper ">=" arg1 arg2
          | f == "(||)" -> helper "||" arg1 arg2
          | f == "(&&)" -> helper "&&" arg1 arg2
        IntLN i -> show i
        BoolLN b ->
          if b
            then "true"
            else "false"
        FloatLN f -> show f
        DoubleLN d -> show d
        VarN vid ->
          if vid == -1
            then "arg"
            else "arg" ++ show vid
        FuncN c a -> "func" ++ show c ++ "(" ++ genPrimExp nodes a ++ ")"
        _ -> "n" ++ show nid -- arguments of closure

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0 ..]

gen ::
     [(String, NodeId)]
  -> Map NodeId [B.Timing]
  -> Map NodeId Node
  -> Map NodeId TypeTag
  -> [NodeId]
  -> Map NodeId TypeTag
  -> Set M.ClozId
  -> Map NodeId M.ClozId
  -> Map M.ClozId [NodeId]
  -> [NodeId]
  -> String
  -> String
gen outs inps nodes types lifted argType clozs ntc ioc pushOrder bt =
  unlines $
  DL.toList $
  DL.fromList prelude <> genInputNodes (Map.keys inps) nodes <>
  genLiftedFuncs lifted nodes types ioc argType <>
  genPrimClozs clozs nodes types ntc ioc <>
  genMain outs inps nodes types ntc ioc pushOrder bt
  where
    prelude = [ "#include \"runtime/runtime_" ++ bt ++ ".h\""
              , "using hae::IN__fps;"]

genInputNodes :: [NodeId] -> Map NodeId Node -> DL.DList String
genInputNodes nids nodes = DL.concat $ Prelude.map f nids
  where
    f nid =
      case nodes ! nid of
        InputN "_fps" tm -> DL.empty
        InputN name _ ->
          DL.fromList
            [ printf "class IN_%s : public hae::InputNode {" name
            , "  public:"
            , printf "    IN_%s() : InputNode() {}" name
            , "  protected:"
            , "    virtual hae::Event process() {"
            , ""
            , "    }"
            , "};"
            ]

genLiftedFuncs ::
     [NodeId]
  -> Map NodeId Node
  -> Map NodeId TypeTag
  -> Map M.ClozId [NodeId]
  -> Map NodeId TypeTag
  -> DL.DList String
genLiftedFuncs lifted nodes types ioc argType = Prelude.foldr step DL.empty lifted
  where
    step cid acc =
      let typeStr = typeTag2cppStr $ types ! cid
          argTypeStr = typeTag2cppStr $ argType ! cid
       in DL.fromList
            [ typeStr ++ " func" ++ show cid ++ "(" ++ argTypeStr ++ "narg) {"
            , "  return " ++ genPrimExp nodes cid ++ ";"
            , "}"
            ] <>
          acc

genPrimClozs ::
     Set M.ClozId
  -> Map NodeId Node
  -> Map NodeId TypeTag
  -> Map NodeId M.ClozId
  -> Map M.ClozId [NodeId]
  -> DL.DList String
genPrimClozs clozs nodes types ntc ioc = Set.foldr step DL.empty clozs
  where
    step cid acc =
      DL.fromList
        [ "class PN" ++ show cid ++ " : public hae::PrimNode {"
        , "  public:"
        , "    PN" ++ show cid ++ "(int argc) : PrimNode(argc) {}"
        , "  protected:"
        , "    virtual hae::value f_(std::vector<hae::value> argv) {"
        ] <>
      genArgsExt (ioc ! cid) <>
      DL.fromList
        [ "        return hae::value(" ++ genPrimExp nodes cid ++ ");"
        , "    }"
        , "};"
        ] <>
      acc
    genArgsExt :: [NodeId] -> DL.DList String
    genArgsExt nids =
      DL.singleton
        "        std::vector<hae::value>::const_iterator it = argv.begin();" <>
      DL.fromList (Prelude.map f nids)
      where
        f nid =
          case types ! nid of
            TInt -> "        int n" ++ show nid ++ " = (it++)->int_value;"
            TBool -> "        bool n" ++ show nid ++ " = (it++)->bool_value;"
            TFloat -> "        float n" ++ show nid ++ " = (it++)->float_value;"
            TDouble ->
              "        double n" ++ show nid ++ " = (it++)->double_value;"
            _ -> undefined

-- TODO: this piece of code smells
genMain ::
     [(String, NodeId)]
  -> Map NodeId [B.Timing]
  -> Map NodeId Node
  -> Map NodeId TypeTag
  -> Map NodeId M.ClozId
  -> Map M.ClozId [NodeId]
  -> [NodeId]
  -> String
  -> DL.DList String
genMain outs inps nodes types ntc ioc pushOrder bt =
  mainPrelude bt <> genNodeDefs <> mainFinale
  where
    mainPrelude "mbed" =
      DL.fromList ["int main() {", "    hae::MBedEngineImpl engine;"]
    mainPrelude "debug" =
      DL.fromList ["int main() {", "    hae::DebugEngineImpl engine;"]
    mainFinale = DL.fromList ["    engine.dispatch();", "    return 0;", "}"]
    genNodeDefs = DL.fromList $ Prelude.concatMap f pushOrder
      where
        f nid =
          case nodes ! nid of
            InputN name _ ->
              printf "    IN_%s n%d;" name nid :
              Prelude.concat
                (mapInd
                   (\t i ->
                      case t of
                        B.NormalTiming n ->
                          [ printf
                              "    n%d.add_timing(hae::Timing(%d, 0, 0));"
                              nid
                              n
                          ]
                        B.PastTiming n ps ->
                          [ printf
                              "    hae::ttime t%d_%d[%d] = {%s};"
                              nid
                              i
                              (Set.size ps)
                              (B.listPastTimingStr ps)
                          , printf
                              "    n%d.add_timing(hae::Timing(%d, %d, t%d_%d));"
                              nid
                              n
                              (Set.size ps)
                              nid
                              i
                          ])
                   (inps ! nid)) ++
              ["    engine.register_input_node(" ++ "&n" ++ show nid ++ ");"]
            Prim1N {} ->
              printf "    PN%d n%d(%d);" nid nid (length (ioc ! nid)) :
              pnPushRelation (ioc ! nid) nid
            Prim2N {} ->
              printf "    PN%d n%d(%d);" nid nid (length (ioc ! nid)) :
              pnPushRelation (ioc ! nid) nid
            PrevN p ->
              [ "    hae::PrevNode n" ++ show nid ++ ";"
              , "    n" ++ show nid ++ ".add(&n" ++ show p ++ ");"
              ]
            SampleN tar ref ->
              [ "    hae::SampleNode n" ++ show nid ++ ";"
              , "    n" ++ show nid ++ ".add(&n" ++ show tar ++ ");"
              , "    n" ++ show nid ++ ".add(&n" ++ show ref ++ ");"
              ]
            _ -> undefined
        pnPushRelation pars target =
          Prelude.map
            (\x -> "    n" ++ show target ++ ".add(&n" ++ show x ++ ");")
            pars
