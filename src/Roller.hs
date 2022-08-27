module Roller (rollIO) where

import Relude.Extra(secondF)

import Control.Monad.Writer(WriterT,runWriterT,tell)
import Data.Functor.Foldable(Base,Recursive,project)
import Data.List.Extra(unsnoc)
import Parser(Roll,RollF(..),RerollOpts(..))
import System.Random(StdGen,randomR,getStdRandom)

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

rollIO :: Roll -> IO (Int,Text)
rollIO = secondF format . getStdRandom . runState . runWriterT . rollDice

-- | A monadic catamorphism
cataM :: (Recursive t, Traversable (Base t), Monad m) => (Base t a -> m a) -> (t -> m a)
cataM phi = c where c = phi <=< (traverse c . project)
-- coppied from recursion-scheems-ext I would've imported but it seems not to compile

rollDice :: Roll -> RollM Int
rollDice = cataM $ \case
  CF n -> pure n
  DF o a b -> sum <$> replicateM a (rollSmpl b o)
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
