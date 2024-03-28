module Sample (rollIO) where

import Control.Monad.Trans.Except (throwE)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Flow ((.>))
import RollM (RollM (..), rollDice)
import System.Random (StdGen, getStdRandom, randomR)
import TypeCheck (Roll)
import Util (joinPair)

newtype Sample a = Sample {runSample :: ExceptT Text (WriterT Text (State StdGen)) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState StdGen
    , MonadWriter Text
    )

rollIO :: (MonadIO m) => Roll -> m (Either Text (Int, Text))
rollIO =
  rollDice
    .> runSample
    .> runExceptT
    .> runWriterT
    .> runState
    .> getStdRandom
    .> fmap joinPair

instance RollM Sample where
  range a b
    | b < a = throw "dice with 0 or fewer sides encountered"
    | otherwise = state $ randomR (a, b)
  times' n f r = do
    rolls <- replicateM (abs n) r
    let res = (signum n *) . sum $ f <$> rolls
    log $ case rolls of
      [x] -> show x <> "\n"
      xs -> show xs <> "=" <> show res <> "\n"
    pure res
  log = tell
  throw = Sample . throwE
