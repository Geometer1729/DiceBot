module Sample(rollIO)where

import Control.Monad.Writer (WriterT, runWriterT, tell, MonadWriter)
import System.Random (StdGen, getStdRandom, randomR)
import Control.Monad.Trans.Except(throwE)
import RollM (RollM(..),rollDice)
import Parser (Roll)
import Flow((.>))

newtype Sample a = Sample{runSample :: ExceptT Text (WriterT Text (State StdGen)) a}
  deriving newtype
    (Functor
    ,Applicative
    ,Monad
    ,MonadState StdGen
    ,MonadWriter Text
    )

rollIO :: MonadIO m => Roll -> m (Either Text (Int, Text))
rollIO =
    rollDice
    .> runSample
    .> runExceptT
    .> runWriterT
    .> runState
    .> getStdRandom
    .> fmap (\(a,b) -> a <&> (,b))

instance RollM Sample where
  range a b
      | b < a = throw "dice with 0 or fewer sides encountered"
      | otherwise = state $ randomR (a, b)
  times n r = do
    rolls <- replicateM (abs n) r
    let res = (signum n *) . sum $ rolls
    log $ case rolls of
      [x] -> show x <> "\n"
      xs -> show xs <> "=" <> show res <> "\n"
    pure res
  log = tell
  throw = Sample . throwE

