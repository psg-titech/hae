module Hae.Internal.FOAS where

import Data.Typeable (TypeRep, splitTyConApp, tyConName)
import Hae.Internal.Expr (TimingDef(..))
import Hae.Types

type NodeId = Int

data TypeTag
  = TInt
  | TBool
  | TFloat
  | TDouble
  | TPair TypeTag
          TypeTag
  | TLam TypeTag
         TypeTag
  deriving (Show, Eq)

typeRep2TypeTag :: TypeRep -> TypeTag
typeRep2TypeTag = go . stripOpenExpr
  where
    stripOpenExpr = (!! 1) . snd . splitTyConApp
    go tr =
      case splitTyConApp tr of
        (tyCon, []) ->
          case tyConName tyCon of
            "Int" -> TInt
            "Bool" -> TBool
            "Float" -> TFloat
            "Double" -> TDouble
        (tyCon, tyRepLst) ->
          case tyConName tyCon of
            "(,)" -> TPair (go $ head tyRepLst) (go $ tyRepLst !! 1)
            "(->)" -> TLam (go $ head tyRepLst) (go $ tyRepLst !! 1)

data Node
  = Prim1N String
           NodeId
  | Prim2N String
           NodeId
           NodeId
  | SwitchN NodeId
            NodeId
            NodeId
  | InputN String
           TimingDef
  | IntLN Int
  | BoolLN Bool
  | FloatLN Float
  | DoubleLN Double
  | PairN NodeId
          NodeId
  | FuncN NodeId -- |Nid of lambda
          NodeId -- Nid of arg
  | SampleN NodeId -- |Target
            NodeId -- |Ref
  | PrevN NodeId
  | VarN NodeId

-- |simple show instance
instance Show Node where
  show (IntLN i) = show i
  show (BoolLN b) = show b
  show (FloatLN f) = show f
  show (DoubleLN d) = show d
  show (PairN n1d n2d) = "(n" ++ show n1d ++ ", n" ++ show n2d ++ ")"
  show (InputN tags timing) =
    "input:" ++ show tags ++ " " ++ show timing
  show (Prim1N fid aid) = show fid ++ " n" ++ show aid
  show (Prim2N fid a1id a2id) =
    show fid ++ " n" ++ show a1id ++ " n" ++ show a2id
  show (SwitchN pid tid fid) =
    "if n" ++ show pid ++ " then n" ++ show tid ++ " else n" ++ show fid
  show (FuncN n aid) = "n" ++ show n ++ " |$| n" ++ show aid
  show (SampleN n1d n2d) = "sample n" ++ show n1d ++ " on n" ++ show n2d
  show (PrevN nid) = "prev n" ++ show nid
  show (VarN nid) = "var n" ++ show nid
