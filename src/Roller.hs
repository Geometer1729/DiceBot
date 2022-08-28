module Roller (rollIO) where

import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Functor.Foldable (Base, Recursive, project)
import Parser (RerollOpts (..),RerollBest(..),RerollUnder(..), Roll, RollF (..),Dir(..))
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
    tell $ case a of
      1 -> "d" <> show b <> show o <> "= "
      _ -> show a <> "d" <> show b <> show o <> "= "
    rolls <- replicateM a $ rollSmpl b o
    let res = sum rolls
    tell $ case rolls of
      [x] ->  show x <> "\n"
      xs -> show xs <> "=" <> show res <> "\n"
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
         Just (RerollBest dir a b) -> do
           let sorter = case dir of
                      Best -> sortOn Down
                      Worst -> sort
           (keep,toss) <- splitAt b . sorter <$> replicateM a withUnder
           tell $ " ~~" <> show toss <> "~~ "
           tell $ show keep
           pure $ sum keep

    withUnder :: RollM Int
    withUnder = case under of
                  Nothing -> d n
                  Just (Under a) -> range (a+1) n
                  Just (OnceUnder a) -> do
                    res <- d n
                    if res <= a
                       then do
                         tell $ "~~" <> show res <> "~~ "
                         d n
                       else pure res

    range :: Int -> Int -> RollM Int
    range a b = state $ randomR (a, b)

    d :: Int -> RollM Int
    d = range 1
