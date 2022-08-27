module Roller (rollIO) where

import Control.Monad.Writer(WriterT,runWriterT,tell)
import Data.Functor.Foldable(Base,Recursive,project)
import Parser(Roll,RollF(..),RerollOpts(..))
import System.Random(StdGen,randomR,getStdRandom)

type RollM = WriterT Text (State StdGen)

rollIO :: MonadIO m => Roll -> m (Int,Text)
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
      [x] -> "d" <> show b <> "=" <> show x <> "\n"
      xs -> show a <> "d" <> show b <> "=" <> show res <> " " <> show xs <> "\n"
    pure res
  AddF a b -> pure $ a + b
  MulF a b -> pure $ a * b
  DivF a b -> pure $ a `div` b
  SubF a b -> pure $ a - b

rollSmpl :: Int -> RerollOpts -> RollM Int
rollSmpl n = \case
  Dont -> d n
  UnderMin l -> range l n
  OnceUnderMin l -> do
    r1 <- d n
    if r1 <= l
      then d n
      else pure r1
  BestOf amt keeping ->
    sum . take keeping . sortOn Down <$> replicateM amt (d n)
  WorstOf amt keeping ->
    sum . take keeping . sort <$> replicateM amt (d n)
  where
    range :: Int -> Int -> RollM Int
    range a b = state $ randomR (a,b)

    d :: Int -> RollM Int
    d = range 1
