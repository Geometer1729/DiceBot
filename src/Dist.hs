module Dist
  (Dist
  ,range
  ,d
  ,size
  ,times
  ,times'
  ,toMap
  ,Dist.toList
  ,toDist
  ,fromMap
  ,expected
  ,chanceOf
  ,maybeIn
  ,maybeOut
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
import Util (joinPair)

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

d :: Int -> Maybe (Dist Int)
d = range 1

range :: Int -> Int -> Maybe (Dist Int)
range a b =
  let p = 1/fromIntegral (b-a+1)
   in guard (a<=b) $> Dist [(i,p) | i <- [a..b]]

toDist :: Foldable f => f (a,Double) -> Dist a
toDist = Prelude.toList .> Dist .> msimple

fromMap :: Map a Double -> Dist a
fromMap = Map.toList .> Dist

toList :: Ord a => Dist a -> [(a,Double)]
toList = simple .> unDist

toMap :: Ord a => Dist a -> Map a Double
toMap = unDist .> fromListWith (+)

size :: Ord a => Dist a -> Int
size = toMap .> length

times :: (Ord a,Num a) => Int -> Dist a -> Dist a
times = times' 0 (+)

-- | Add together identical distributions
times' :: Ord a => a -> (a -> a -> a) -> Int -> Dist a -> Dist a
times' m f = let
  go 0 _ = pure m
  go 1 di = di
  go n di = let
    !di' = simple $ go (n `div` 2) di
    fm = fmap simple . liftM2 f
      in di' `seq` fm di' $ fm di' (simple $ go (n `mod` 2) di)
    in go

expected :: Dist Int -> Double
expected = toMap .> Map.toList .> map (\(n,p) -> fromIntegral n * p) .> sum

chanceOf :: Ord a => (a -> Bool) -> Dist a -> Double
chanceOf predicate = toMap .> Map.filterWithKey (flip $ const predicate) .> sum

maybeIn :: Maybe (Dist a) -> Dist (Maybe a)
maybeIn Nothing = pure Nothing
maybeIn (Just di) = Just <$> di

maybeOut :: Ord a => Dist (Maybe a) -> Maybe (Dist a)
maybeOut = simple .> unDist .> traverse joinPair .> fmap Dist

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
#-} -- TODO why won't it parse if they are one pragma?
{-# RULES
   "simpleMInts" msimple @(Maybe Int) = simple
#-}
