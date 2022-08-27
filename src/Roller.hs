module Roller (rollIO) where

import Relude.Extra(secondF)

import Parser(Roll,Fix(..),RollF(..),RerollOpts(..))
import System.Random(StdGen,randomR,getStdRandom)
import Control.Monad(liftM2)
import Control.Monad.Writer(WriterT,runWriterT,tell)
import Data.List.Extra(unsnoc)

type Alg f a = f a -> a

type RollM = WriterT RollLogs (State StdGen)

newtype RollLogs = RL{unRL :: [Ent]}
data Ent = Ent{sides::Int,got::[Int]}

instance Semigroup RollLogs where
  x <> (RL []) = x
  (RL (unsnoc -> Nothing)) <> x = x
  (RL (unsnoc -> Just(xs,x))) <> (RL (y:ys))
    | sides x == sides y = RL $ xs ++ [Ent (sides y) (got x ++ got y)] ++ ys
    | otherwise = RL $ xs ++ (x:y:ys)
  (RL _) <> _ = error "unreachable"

instance Monoid RollLogs where
  mempty = RL []

format :: RollLogs -> Text
format = unlines . map formatEnt . unRL
  where
    formatEnt :: Ent -> Text
    formatEnt (Ent _ []) = error "logs were bad"
    formatEnt (Ent n [x]) = "d" <> show n <> "=" <> show x
    formatEnt (Ent n xs) = "d" <> show n <> "=" <> show xs

cata :: Functor f => Alg f a -> Fix f -> a
cata f = f . fmap (cata f) . outF

rollIO :: Roll -> IO (Int,Text)
rollIO = secondF format . getStdRandom . runState . runWriterT . rollDice

rollDice :: Roll -> RollM Int
rollDice = cata $ \case
  C n -> pure n
  D o a b -> sum <$> (replicateM ==<< a) (rollSmpl ==<< b $ o)
  Add a b -> liftM2 (+) a b
  Mul a b -> liftM2 (*) a b
  Div a b -> liftM2 div a b
  Sub a b -> liftM2 (-) a b

(==<<) :: Monad m => (a -> b -> m c) -> m a -> b -> m c
f ==<< m = \x -> m >>= (`f` x)

rollSmpl :: Int -> RerollOpts -> RollM Int
rollSmpl n = \case
  Dont -> d n
  UnderMin l -> range l n
  OnceUnderMin l -> do
    r1 <- d n
    if r1 <= l
      then d n
      else pure r1
  BestOf amt keeping -> sum . take keeping . sortOn Down <$> replicateM amt (d n)
  WorstOf amt keeping -> sum . take keeping . sort <$> replicateM amt (d n)
  where
    range :: Int -> Int -> RollM Int
    range a b = do
      res <- state $ randomR (a,b)
      tell $ RL[Ent n [res]]
      pure res

    d :: Int -> RollM Int
    d = range 1
