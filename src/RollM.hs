module RollM (rollDice, RollM (..)) where

import TypeCheckerCore (ExprT (..), HRefable (..), HsOf)

class (Monad m) => RollM m where
  range :: Int -> Int -> m Int
  log :: Text -> m ()
  throw :: Text -> m a
  times :: Int -> m Int -> m Int
  times n = times' n id

  times' :: Int -> ([Int] -> [Int]) -> m Int -> m Int

d :: (RollM r) => Int -> r Int
d = range 1

rollDice :: forall r d t. (RollM r, HsOf d ~ t) => ExprT d -> r t
rollDice = \case
  Hask (HRef _ l) -> pure l
  App f x -> rollDice f <*> rollDice x
  Dice a b (Just k) -> do
    a' <- rollDice a
    b' <- rollDice b
    k' <- rollDice k
    unless (k' <= a') $ throw $
      "can't keep " <> show k'
      <> " of " <> show a'
    log $ case a' of
      1 -> "d" <> show b' <> "k" <> show k' <>"= "
      _ -> show a' <> "d" <> show b' <> "= "
    times' a' (take k' . sortOn Down) (d b')
  Dice a b Nothing -> do
    a' <- rollDice a
    b' <- rollDice b
    log $ case a' of
      1 -> "d" <> show b' <> "= "
      _ -> show a' <> "d" <> show b' <> "= "
    a' `times` d b'
