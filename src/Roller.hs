module Roller (rollIO) where

import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Functor.Foldable (Base, Recursive, project)
import Parser (RerollOpts (..),RerollBest(..),RerollUnder(..), Roll, RollF (..))
import System.Random (StdGen, getStdRandom, randomR)

type RollM = WriterT Text (State StdGen)

rollIO :: MonadIO m => Roll -> m (Int, Text)
rollIO = getStdRandom . runState . runWriterT . rollDice

-- | A monadic catamorphism
cataM :: (Recursive t, Traversable (Base t), Monad m) => (Base t a -> m a) -> (t -> m a)
cataM phi = c where c = phi <=< (traverse c . project)

-- coppied from recursion-scheems-ext I would've imported but it seems not to compile

rollDice :: Roll -> RollM Int
rollDice = cataM $ \case
  CF n -> pure n
  DF o a b -> do
    rolls <- replicateM a $ rollSmpl b o
    let res = sum rolls
    tell $ case rolls of
      [x] -> "d" <> show b <> show o <> "=" <> show x <> "\n"
      xs -> show a <> "d" <> show b <> "=" <> show res <> " " <> show xs <> "\n"
    pure res
  AddF a b -> pure $ a + b
  MulF a b -> pure $ a * b
  DivF a b -> pure $ a `div` b
  SubF a b -> pure $ a - b

rollSmpl :: Int -> RerollOpts -> RollM Int
rollSmpl n RerollOpts{..} = withBest
  where
    withBest :: RollM Int
    withBest =
      case best of
         Nothing -> withUnder
         Just (Best a b) -> sum . take a . sortOn Down <$> replicateM b withUnder
         Just (Worst a b) -> sum . take a . sort <$> replicateM b withUnder

    withUnder :: RollM Int
    withUnder = case under of
                  Nothing -> d n
                  Just (Under a) -> range a n
                  Just (OnceUnder a) -> do
                    res <- d n
                    if res <= a
                       then d n
                       else pure res

    range :: Int -> Int -> RollM Int
    range a b = state $ randomR (a, b)

    d :: Int -> RollM Int
    d = range 1
