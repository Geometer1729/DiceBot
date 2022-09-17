module Expect
  (report
  ,showAmt
  )
    where


import Parser

import Data.Text qualified as T

import Control.Monad (liftM2)
import Data.FormatN (percent, commaSF,fixed)
import Data.Functor.Foldable (cata)
import Dist(Dist, times, range, d,expected,chanceOf)
import Flow((.>))

report :: Roll -> Int -> Text
report r res = let
  !dist = toDist r
  e = expected dist
  c = chanceOf  (== res) dist
  cb = chanceOf (> res) dist
  cl = chanceOf (< res) dist
  delta = fromIntegral res - e
     in "expected: " <> showAmt e <> (if delta > 0 then " (+" else " (") <> showAmt delta <> ")"
      <> "\nchance: " <> showChance c
      <> case compare cb cl of
           GT -> "\nchance of result this low:" <> showChance (cl +c)
           EQ -> "\nmedian roll"
           LT -> "\nchance of a roll this high:" <> showChance (cb + c)

showAmt :: Double -> Text
showAmt = fixed  (Just 5) .> trimZeros

trimZeros :: Text -> Text
trimZeros w =
  if not $ '.' `T.elem` w
     then w
     else T.dropWhileEnd (== '.') $ T.dropWhileEnd (== '0') w

showChance :: Double -> Text
showChance = percent commaSF (Just 3)

-- TODO
-- this is pretty repeditive with Roller
-- I probably want a general class where
-- this code could be the same


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


