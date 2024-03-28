{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeCheckerCore where

import Data.Singletons.TH

-- TODO
-- polymorphic types
-- if statments
-- lambda functions
-- add back rerolls

$( singletons
    [d|
      data DType
        = DInt
        | DBool
        | DList DType
        | DFun DType DType
      |]
 )

-- \| DVar Natural

$(singDecideInstance ''DType)

deriving stock instance Show DType
deriving stock instance Eq DType
deriving stock instance Ord DType

deriving stock instance Show (SDType DInt)
deriving stock instance Show (SDType DBool)
deriving stock instance (Show (SDType d)) => Show (SDType (DList d))
deriving stock instance (Show (SDType d1), Show (SDType d2)) => Show (SDType (DFun d1 d2))

type HsOf :: DType -> Type
type family HsOf (d :: DType) where
  HsOf DInt = Int
  HsOf DBool = Bool
  HsOf (DList d) = [HsOf d]
  HsOf (DFun a b) = HsOf a -> HsOf b

data ExprT (d :: DType) where
  Lit :: HsOf t -> ExprT t
  App :: ExprT (DFun a b) -> ExprT a -> ExprT b
  Dice :: ExprT DInt -> ExprT DInt -> ExprT DInt

-- FreeVar :: forall (n::Text) (d :: DType). ExprT  d
-- Lambda :: SubScope so si => ExprT ('(n,d) ': si) t -> ExprT so (DFun d t)

data Res where
  Res :: (SingI d) => ExprT d -> Res
