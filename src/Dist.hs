module Dist
  (Dist
  ,range
  ,d
  ,size
  ,times
  ,toMap
  ,toDist
  ,fromMap
  ,expected
  ,chanceOf
  -- | unsafe
  ,unsafeSize
  ,unsafeToList
  )
  where

import Data.Map.Strict qualified as Map
import Data.List qualified as List

import Flow((.>),(<.),(|>))
import Data.Map.Strict (fromListWith)
import Control.Monad (liftM2)
import GHC.Show qualified as Show

newtype Dist a = Dist{unDist :: [(a,Double)]}

instance (Ord a,Show a) => Show (Dist a) where
  show = toMap .> show

instance Functor Dist where
  {-# INLINE fmap #-}
  fmap f x = msimple (x |> unDist .> map (first f) .> Dist)

instance Applicative Dist where
  pure = Dist <. pure <. (,1)
  {-# INLINE (<*>) #-}
  (Dist f) <*> (Dist x) =
    msimple $ Dist
      [ (f' x',p1*p2) | (f',p1) <- f , (x',p2) <- x ]

instance Monad Dist where
  {-# INLINE (>>=) #-}
  x >>= f = msimple $ Dist $ do
    (x',p1) <- unDist x
    (y,p2) <- unDist $ f x'
    pure (y,p1*p2)

instance Num a => Num (Dist a) where
  {-# INLINE fromInteger #-}
  fromInteger = fromInteger .> pure
  {-# INLINE (+) #-}
  (+) = liftM2 (+)
  {-# INLINE (-) #-}
  (-) = liftM2 (-)
  {-# INLINE (*) #-}
  (*) = liftM2 (*)
  {-# INLINE abs #-}
  abs = fmap abs
  {-# INLINE signum #-}
  signum = fmap signum

d :: Int -> Dist Int
d = range 1

range :: Int -> Int -> Dist Int
range a b =
  let p = 1/fromIntegral (b-a+1)
   in Dist [(i,p) | i <- [a..b]]

toDist :: Foldable f => f (a,Double) -> Dist a
toDist = toList .> Dist .> msimple

fromMap :: Map a Double -> Dist a
fromMap = Map.toList .> Dist

toMap :: Ord a => Dist a -> Map a Double
toMap = unDist .> fromListWith (+)

size :: Ord a => Dist a -> Int
size = toMap .> length

-- | Add together identical distributions
{-# INLINE times #-}
times :: Num a => Int -> Dist a -> Dist a
times 0 _ = 0
times 1 di = di
times n di = let
  !di' = msimple $ times (n `div` 2) di
    in di' `seq` (di' + di') + times (n `mod` 2) di

expected :: Dist Int -> Double
expected = toMap .> Map.toList .> map (\(n,p) -> fromIntegral n * p) .> sum

chanceOf :: Ord a => (a -> Bool) -> Dist a -> Double
chanceOf predicate = toMap .> Map.filterWithKey (flip $ const predicate) .> sum

-- unsafe
-- these functions are unsafe because
-- they expose unlawful behavior
-- ie. fmap id /= id
-- but the other functions shouldn't allow you to inspect
-- a dist in a way that would make this discoverable

-- usefull for testing the rules
unsafeSize :: Dist a -> Int
unsafeSize = unDist .> length

unsafeToList :: Dist a -> [(a,Double)]
unsafeToList = unDist

{-# NOINLINE msimple #-}
msimple :: Dist a -> Dist a
msimple = id

simple :: Ord a => Dist a -> Dist a
simple =
  unDist
  .> sort
  .> List.groupBy ((==) `on` fst)
  .> map ((List.head .> fst) &&& (map snd .> sum))
  .> Dist

-- TODO look into auto generating rules with template haskell

{-# RULES
 "simpleInts" msimple @Int = simple
#-}
