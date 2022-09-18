module Expect
  (report
  ,showAmt
  )
    where


import Parser

import Data.Text qualified as T

import Control.Monad (liftM2)
import Data.FormatN (percent, commaSF,fixed)
import Dist(Dist, times, range, d,expected,chanceOf, maybeIn, maybeOut)
import Flow((.>))
import Roller (cataM)

report :: Roll -> Int -> Text
report r res =
  case toDist r of
    Nothing -> "stats unavailable because the expression had some chance of invalid dice"
    Just !dist ->
      let
        e = expected dist
        c = chanceOf  (== res) dist
        cb = chanceOf (>= res) dist
        cl = chanceOf (<= res) dist
        delta = fromIntegral res - e
           in "expected: " <> showAmt e
            <> "\ngot: " <> show res <> (if delta >= 0 then " (+" else " (") <> showAmt delta <> ")"
            <> "\nchance of rolling that: " <> showChance c
            <> if cl > 0.5 &&  cb > 0.5
                then "\nmedian roll"
                else case compare cb cl of
                  GT -> "\nchance of getting a roll this low: " <> showChance cl
                  LT -> "\nchance of getting a roll this high: " <> showChance cb
                  EQ -> "\nmedian roll" -- should be unreachable but median would be correct here

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


toDist :: Roll -> Maybe (Dist Int)
toDist = cataM $ \case
  CF n -> Just $ pure n
  AddF a b -> pure $ a + b
  SubF a b -> pure $ a - b
  MulF a b -> pure $ a * b
  DivF a b -> pure $ liftM2 div a b -- TODO should this be Rational
  DF opts a' b' -> maybeOut $ do
    a <- a'
    b <- b'
    maybeIn $
      fmap (signum a *) . (abs a `times`) <$> rollSmpl b opts

rollSmpl :: Int -> RerollOpts -> Maybe (Dist Int)
rollSmpl n RerollOpts{..} = withBest
  where
    withBest :: Maybe (Dist Int)
    withBest =
      case best of
        Nothing -> withUnder
        Just (RerollBest dir a b) ->
            withUnder <&> \valid -> do
                 let sorter = case dir of
                            Best -> sortOn Down
                            Worst -> sort
                 (keep,_toss) <- splitAt b . sorter <$> replicateM a valid
                 pure $ sum keep

    withUnder :: Maybe (Dist Int)
    withUnder = case under of
                  Nothing -> d n
                  Just (Under a) -> range (a+1) n
                  Just (OnceUnder a) -> do
                    case d n of
                      Nothing -> Nothing
                      Just dn -> maybeOut $ do
                        res <- dn
                        if res <= a
                           then do
                             maybeIn $ d n
                           else pure $ Just res


