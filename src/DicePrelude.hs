module DicePrelude where

import Data.Map qualified as M
import Data.Singletons (SingI)
import TypeCheckerCore (DType (..), ExprT (Lit), HsOf, Res (Res))

class (HsOf (DOf l) ~ l, SingI (DOf l)) => Lit l where
  type DOf l :: DType
  lit :: l -> Res
  lit = Res @(DOf l) . lit'
  lit' :: l -> ExprT (DOf l)
  lit' = Lit

instance Lit Int where
  type DOf Int = DInt

instance Lit Bool where
  type DOf Bool = DBool

instance (Lit a, Lit b) => Lit (a -> b) where
  type DOf (a -> b) = DFun (DOf a) (DOf b)

prelude :: Map Text Res
prelude =
  M.fromList
    [ ("+", lit @(Int -> _) (+))
    , ("-", lit @(Int -> _) (-))
    , ("*", lit @(Int -> _) (*))
    , ("/", lit @(Int -> _) div)
    , ("^", lit @(Int -> Int -> Int) (^))
    , ("**", lit @(Int -> Int -> Int) (^))
    , ("&&", lit @(Bool -> _) (&&))
    , ("||", lit @(Bool -> _) (&&))
    ]
