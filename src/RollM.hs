module RollM (rollDice, RollM (..)) where

import TypeCheckerCore (ExprT (..), HsOf,DType(DVar), HRefable (..))

class (Monad m) => RollM m where
  range :: Int -> Int -> m Int
  log :: Text -> m ()
  throw :: Text -> m a
  times :: Int -> m Int -> m Int
  times n = times' n id

  times' :: Int -> (Int -> Int) -> m Int -> m Int
  times' n f m = times n (f <$> m)

d :: (RollM r) => Int -> r Int
d = range 1

rollDice :: forall r d t. (RollM r, HsOf d ~ t) => ExprT d -> r t
rollDice = \case
  Hask (HRef _ l) -> pure l
  App f x -> rollDice f <*> rollDice x
  Dice a b -> do
    a' <- rollDice a
    b' <- rollDice b
    log $ case a' of
      1 -> "d" <> show b' <> "= "
      _ -> show a' <> "d" <> show b' <> "= "
    a' `times` d b'
