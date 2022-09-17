module Expect
  (report)
    where

import Data.Functor.Foldable (cata)
import Dist(Dist, times, range, d,expected,chanceOf)

import Parser
import Control.Monad (liftM2)

-- TODO
-- this is pretty repeditive with Roller
-- I probably want a general class where
-- this code could be the same

report :: Roll -> Int -> Text
report r res = let
  !dist = toDist r
  e = expected dist
  c = chanceOf  (== res) dist
  cb = chanceOf (> res) dist
  cl = chanceOf (< res) dist
     in "expected: " <> show e
      <> "\nchance: " <> showChance c
      <> case compare cb cl of
           GT -> "\nchance of result this low:" <> showChance (cl +c)
           EQ -> "\nmedian roll"
           LT -> "\nchance of a roll this high:" <> showChance (cb + c)

showChance :: Double -> Text
showChance c = show (c*100) <> "%"

toDist :: Roll -> Dist Int
toDist = cata $ \case
  CF n -> pure n
  AddF a b -> a + b
  SubF a b -> a - b
  MulF a b -> a * b
  DivF a b -> liftM2 div a b -- TODO should this be Rational
  DF opts a' b' -> do
    a <- a'
    b <- b'
    a `times` rollSmpl b opts

rollSmpl :: Int -> RerollOpts -> Dist Int
rollSmpl n RerollOpts{..} = withBest
  where
    withBest :: Dist Int
    withBest =
      case best of
        Nothing -> withUnder
        Just (RerollBest dir a b) -> do
           let sorter = case dir of
                      Best -> sortOn Down
                      Worst -> sort
           (keep,_toss) <- splitAt b . sorter <$> replicateM a withUnder
           pure $ sum keep

    withUnder :: Dist Int
    withUnder = case under of
                  Nothing -> d n
                  Just (Under a) -> range (a+1) n
                  Just (OnceUnder a) -> do
                    res <- d n
                    if res <= a
                       then do
                         d n
                       else pure res


