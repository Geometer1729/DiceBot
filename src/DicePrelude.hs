{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module DicePrelude where

import Data.Map qualified as M
import TypeCheckerCore (DType (..), ExprT (Hask), Res (Res), HRefable (..), Ref, sRef, HsOf)
import Prelude.Singletons (withSingI)
import Data.Singletons (SingI(..), Sing)


prelude :: Map Text Res
prelude =
  M.fromList
    [ ("+", easy $ (+) @Int)
    , ("-", easy $ (-) @Int)
    , ("*", easy $ (*) @Int)
    , ("/", easy $ div @Int)
    , ("^", easy $ (^) @Int @Int)
    , ("**", easy $ (^) @Int @Int)
    , ("&&", easy (&&))
    , ("||", easy (||))
    -- TODO classes so you can compare lists and such?
    , (">", easy $ (>) @Int)
    , ("<", easy $ (<) @Int)
    , (">=", easy $ (>=) @Int)
    , ("<=", easy $ (<=) @Int)
    , ("==", easy $ (==) @Int)
    , ("/=", easy $ (/=) @Int)
    , ("++", easyP @([H0] -> [H0] -> [H0]) (++))
    , ("ifte", easyP @(Bool -> H0 -> H0 -> H0)
        (\c t f -> if c then t else f))
    , ("True" , easy True)
    , ("False" , easy False)
    , ("replicate" , easyP @(Int -> H0 -> [H0]) replicate)
      -- TODO probably better if this works more like replicateM
      -- may require seperate builtin?
    , ("even" , easy @(Int -> Bool)  even)
    , ("odd" , easy @(Int -> Bool)  odd)
    , ("id", easyP @(H0 -> H0) id)
    , ("length" , easyP @([H0] -> Int) length)
    , ("filter" , easyP @((H0 -> Bool) -> [H0] -> [H0]) filter)
    -- TODO more prelude functions
    -- especially (.)
    ]


easyP :: forall h.
   (SingI (DOf h) ,Just0 (DOf h)) => (forall d. HsOf (Ref 0 d (DOf h))) -> Res
easyP h =  Res @(DOf h) $ buildPoly @h (\(Proxy :: Proxy d) -> h @d)

buildPoly :: forall h . (SingI (DOf h) ,Just0 (DOf h)) =>
             (forall d. Proxy d -> HsOf (Ref 0 d (DOf h))) -> ExprT (DOf h)
buildPoly h = Hask $ HRef p (h $ Proxy @(DVar 0))
  where
    p :: forall n t. (SingI n,SingI t) =>
        Proxy '(n,t) -> HRefable (Ref n t (DOf h))
    p Proxy = case sRef (sing @n) (sing @t) (sing @(DVar 0)) of
      (t' :: Sing t') -> withSingI t' $ case j01 @(DOf h) (Proxy @'(n,t)) of
        J01 -> go (Proxy @t')

    go :: forall t. SingI t => Proxy t -> HRefable (Ref 0 t (DOf h))
    go Proxy = HRef
      (\(Proxy :: Proxy '(n1,t1)) -> case sRef (sing @n1) (sing @t1) (sing @t) of
          (t' :: Sing t') -> case j02 @(DOf h) (Proxy @'(n1,t,t1)) of
            J02 -> withSingI t' $ go (Proxy @t')
      )
      (h $ Proxy @t)

data HVar (n :: Natural) where
type H0 = HVar 0
type H1 = HVar 1

type DOf :: Type -> DType
type family DOf (t :: Type) where
  DOf Int = DInt
  DOf Bool = DBool
  DOf [a] = DList (DOf a)
  DOf (a -> b) = DFun (DOf a) (DOf b)
  DOf (HVar n) = DVar n

-- Class for DTypes only polymorphic over DVar 0
class  Ref 0 (DVar 0) d ~ d => Just0 (d :: DType) where
  j01 :: Proxy '(n,t) -> Just0D1 n t d
  j02 :: Proxy '(n,t,t1) -> Just0D2 n t t1 d

data Just0D1 n t (d :: DType) where
  J01 :: Ref 0 (Ref n t (DVar 0)) d ~ Ref n t d => Just0D1 n t d

data Just0D2 n t1 t2 (d :: DType) where
  J02 :: Ref 0 (Ref n t2 t1) d ~ Ref n t2 (Ref 0 t1 d) => Just0D2 n t1 t2 d

instance Just0 (DVar 0) where
  j01 Proxy = J01
  j02 Proxy = J02

instance Just0 DInt where
  j01 Proxy = J01
  j02 Proxy = J02

instance Just0 DBool where
  j01 Proxy = J01
  j02 Proxy = J02

instance Just0 l => Just0 (DList l) where
  j01 p = case j01 @l p of
    J01 -> J01
  j02 p = case j02 @l p of
    J02 -> J02

instance (Just0 a,Just0 b) => Just0 (DFun a b) where
  j01 p = case j01 @a p of
    J01 -> case j01 @b p of
      J01 -> J01
  j02 p = case j02 @a p of
    J02 -> case j02 @b p of
      J02 -> J02

-- Constant under refs
data CRef (d :: DType) (n :: Natural) (t :: DType) where
  CRef :: d ~ Ref n t d => CRef d n t

withCref :: forall d n t a. DEasy d => (Ref n t d ~ d => a) -> a
withCref = case cref @d (Proxy @'(n,t)) of
  CRef -> id

class SingI d => DEasy (d :: DType) where
  cref :: Proxy '(n,t) -> CRef d n t
  easy' :: HsOf d -> ExprT d

easy :: forall h.  (HsOf (DOf h) ~ h, DEasy (DOf h)) => h -> Res
easy a = Res $ easy' @(DOf h) a

instance DEasy DInt where
  cref Proxy = CRef
  easy' n = let go = HRef (\Proxy -> go) n in Hask go

instance DEasy DBool where
  cref Proxy = CRef
  easy' n = let go = HRef (\Proxy -> go) n in Hask go

instance DEasy l => DEasy (DList l) where
  cref (Proxy :: Proxy '(n,t)) = withCref @l @n @t CRef
  easy' n = let go = HRef (\(Proxy :: Proxy '(n,t)) -> withCref @l @n @t go) n in Hask go

instance (DEasy a,DEasy b) => DEasy (DFun a b) where
  cref (Proxy :: Proxy '(n,t)) = withCref @a @n @t $ withCref @b @n @t $ CRef
  easy' n = let go = HRef (\(Proxy :: Proxy '(n,t)) -> withCref @a @n @t $ withCref @b @n @t go) n in Hask go
