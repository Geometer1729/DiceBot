{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeCheckerCore where

import Data.Singletons.TH
import Prelude.Singletons

-- TODO
-- if statments
-- lambda functions
-- add back rerolls

$(singletons
  [d|
     data DType
        = DInt
        | DBool
        | DList DType
        | DFun DType DType
        | DVar Natural

     ref :: Natural -> DType -> DType -> DType
     ref _ _ DInt = DInt
     ref _ _ DBool = DBool
     ref n a (DList l) = DList (ref n a l)
     ref n a (DFun l r) = DFun (ref n a l) (ref n a r)
     ref n a (DVar m) = if n == m then a else DVar m

     refine :: [(Natural,DType)] -> DType -> DType
     refine [] d = d
     refine ((n,t):xs) d = refine xs (ref n t d)

     -- This clumsy inplmentation required for singletons to derive properly
     unify :: DType -> DType -> Maybe [(Natural,DType)]
     unify DInt DInt = Just []
     unify DInt DBool = Nothing
     unify DInt (DList _) = Nothing
     unify DInt (DFun _ _) = Nothing
     unify DInt (DVar n) = Just [(n,DInt)]

     unify DBool DInt = Nothing
     unify DBool DBool = Just []
     unify DBool (DList _) = Nothing
     unify DBool (DFun _ _) = Nothing
     unify DBool (DVar n) = Just [(n,DBool)]

     unify (DFun _ _) DInt = Nothing
     unify (DFun _ _) DBool = Nothing
     unify (DFun _ _) (DList _) = Nothing
     unify (DFun ll lr) (DFun rl rr) = do
        ls <- unify ll rl
        rs <- unify (refine ls lr) (refine ls rr)
        pure $ ls ++ rs
     unify (DFun l r) (DVar n) = Just [(n,DFun l r)]

     unify (DVar n) (DVar m) = Just [(max n m,DVar (min n m))]
     unify (DVar n) DInt = Just [(n,DInt)]
     unify (DVar n) DBool = Just [(n,DBool)]
     unify (DVar n) (DList l) = Just [(n,DList l)]
     unify (DVar n) (DFun a b) = Just [(n,DFun a b)]
     |]
 )

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
  HsOf (DList a) = [HsOf a]
  HsOf (DFun a b) = HsOf a -> HsOf b

data ExprT (d :: DType) where
  Dice :: ExprT DInt -> ExprT DInt -> ExprT DInt
  App :: ExprT (DFun a b) -> ExprT a -> ExprT b
  Hask :: HRefable d -> ExprT d

  -- This may be a good way to do lambdas
  -- Var :: ExprT '[ t ] t
  -- Lambda :: ExprT ( t ': s ) d -> ExprT s (DFun t d)
  -- PopScope :: ExprT s d -> ExprT (t ': s) d

data Res where
  Res :: SingI d => ExprT d -> Res

refOne :: forall n t a. (SingI t,SingI n) => ExprT a -> ExprT (Ref n t a)
refOne = \case
    Dice a b -> Dice a b
    -- Poly p -> p @n @t
    App f x -> App (refOne @n @t f) (refOne @n @t x)
    Hask (HRef f _ :: HRefable a) -> Hask $ f (Proxy @'(n,t))

data FunMaps (refs :: [(Natural,DType)]) (a :: DType) (b :: DType) where
  FunMaps :: Refine refs (DFun a b) ~ DFun (Refine refs a) (Refine refs b) => FunMaps refs a b

funMaps :: forall refs a b. (SingI refs,SingI a,SingI b) => FunMaps refs a b
funMaps = case sing @refs of
  SNil -> FunMaps
  SCons (STuple2 (n :: Sing n) (t :: Sing t)) (refs' :: Sing refs') ->
    withSingI refs' $
    withSingI (sRef n t (sing @a)) $
    withSingI (sRef n t (sing @b)) $
    case funMaps @refs' @(Ref n t a) @(Ref n t b) of
      FunMaps -> FunMaps

-- TODO this doesn't need to be a class anymore
class SingI refs => FinList (refs :: [(Natural,DType)]) where
  refineExpr :: forall a. ExprT a -> ExprT (Refine refs a)

instance SingI refs => FinList refs where
  refineExpr = case sing @refs of
    SNil -> id
    SCons (STuple2 (n :: Sing n) (t :: Sing t)) (refs' :: Sing refs') ->
      withSingI n $ withSingI t $ withSingI refs' $
        refineExpr @refs' . refOne @n @t

data HRefable (d :: DType) where
  HRef ::
    (forall n t. (SingI n,SingI t) => Proxy '(n,t) -> HRefable (Ref n t d)) ->
    (HsOf d) ->
    HRefable d

