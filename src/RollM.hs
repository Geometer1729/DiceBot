module RollM (rollDice, RollM (..)) where

import Data.Functor.Foldable (Base, Recursive, project)
import Parser (Dir (..), RerollBest (..), RerollOpts (..), RerollUnder (..), Roll, RollF (..))

class Monad m => RollM m where
  range :: Int -> Int -> m Int
  times :: Int -> m Int -> m Int
  log :: Text -> m ()
  throw :: Text -> m a

d :: RollM r => Int -> r Int
d = range 1

-- | A monadic catamorphism
cataM :: (Recursive t, Traversable (Base t), Monad m) => (Base t a -> m a) -> (t -> m a)
cataM phi = c where c = phi <=< (traverse c . project)

rollDice :: RollM r => Roll -> r Int
rollDice = cataM $ \case
  CF n -> pure n
  AddF a b -> pure $ a + b
  MulF a b -> pure $ a * b
  DivF a b -> pure $ a `div` b
  SubF a b -> pure $ a - b
  DF o a b -> do
    log $ case a of
      1 -> "d" <> show b <> show o <> "= "
      _ -> show a <> "d" <> show b <> show o <> "= "
    a `times` rollSmpl b o

rollSmpl :: forall r. RollM r => Int -> RerollOpts -> r Int
rollSmpl n RerollOpts {..} = withBest
  where
    withBest :: r Int
    withBest =
      case best of
        Nothing -> withUnder
        Just (RerollBest dir a b) -> do
          let sorter = case dir of
                Best -> sortOn Down
                Worst -> sort
          (keep, toss) <- splitAt b . sorter <$> replicateM a withUnder
          log $ " ~~" <> show toss <> "~~ "
          log $ show keep
          pure $ sum keep

    withUnder :: r Int
    withUnder = case under of
      Nothing -> d n
      Just (Under a) -> range (a + 1) n
      Just (OnceUnder a) -> do
        res <- d n
        if res <= a
          then do
            log $ "~~" <> show res <> "~~ "
            d n
          else pure res
