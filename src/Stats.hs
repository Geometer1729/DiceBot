module Stats (
  genReport,
  -- testing and benchmakring exports
  DistM (runDistM),
  getExpected,
) where

import Parser

import Data.Text qualified as T
import Dist qualified

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (liftM2)
import Data.FormatN (commaSF, fixed, percent)
import Dist (Dist, chanceOf, expected, range)
import Flow ((.>))
import RollM (RollM (..), rollDice)
import Util (joinPair)

newtype DistM a = DistM {runDistM :: MaybeT Dist a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    )

instance RollM DistM where
  range a b = case Dist.range a b of
    Nothing -> throw "encountered invalid dice"
    Just r -> DistM $ lift r
  times n (DistM (MaybeT r)) = DistM $ MaybeT $ Dist.times' (Just 0) (liftM2 (+)) n r
  log _ = pass
  throw _ = DistM $ hoistMaybe Nothing

genReport :: MonadIO m => Roll -> Int -> m (Either (IO (Maybe Text)) Text)
genReport ro res = liftIO $ do
  reportVar <- newEmptyMVar
  computePid <- forkIO $ do
    a <- evaluateNF (report ro res)
    putMVar reportVar (Just a)
  waitPid <- forkIO $ do
    threadDelay 1_000_000
    putMVar reportVar Nothing
    threadDelay 60_000_000
    putMVar reportVar Nothing
  takeMVar reportVar >>= \case
    Nothing -> pure $ Left $
      takeMVar reportVar >>= \case
        Just r -> do
          killThread waitPid
          pure $ Just r
        Nothing -> do
          killThread computePid
          pure Nothing
    Just r -> do
      killThread waitPid
      pure $ Right r

report :: Roll -> Int -> Text
report r res =
  case validate (rollDice r) of
    Left (p, !d) ->
      "This expresion has a " <> showChance p <> " chance of division by 0 or invalid dice\n"
        <> "given a result is valid:\n"
        <> reportDist d res
    Right !dist -> reportDist dist res

getExpected :: Roll -> Maybe Double
getExpected r = do
  Right !d <- pure $ validate $ rollDice r
  pure $ expected d

reportDist :: Dist Int -> Int -> Text
reportDist !dist res =
  let e = expected dist
      c = chanceOf (== res) dist
      cb = chanceOf (>= res) dist
      cl = chanceOf (<= res) dist
      delta = fromIntegral res - e
   in "expected: " <> showAmt e
        <> "\ngot: "
        <> show res
        <> (if delta >= 0 then " (+" else " (")
        <> showAmt delta
        <> ")"
        <> "\nchance of rolling that: "
        <> showChance c
        <> if cl > 0.5 && cb > 0.5
          then "\nmedian roll"
          else case compare cb cl of
            GT -> "\nchance of getting a roll this low: " <> showChance cl
            LT -> "\nchance of getting a roll this high: " <> showChance cb
            EQ -> "\nmedian roll" -- should be unreachable but median would be correct here

validate :: Ord a => DistM a -> Either (Double, Dist a) (Dist a)
validate dm =
  let d' = dm & runDistM .> runMaybeT
      l = d' & Dist.toList .> map joinPair
   in l & sequence .> \case
        Nothing ->
          let p = chanceOf isNothing d'
           in Left (p, l & catMaybes .> map (second (/ (1 - p))) .> Dist.toDist)
        Just xs -> Right $ Dist.toDist xs

showAmt :: Double -> Text
showAmt = fixed (Just 5) .> trimZeros

trimZeros :: Text -> Text
trimZeros w =
  if not $ '.' `T.elem` w
    then w
    else T.dropWhileEnd (== '.') $ T.dropWhileEnd (== '0') w

showChance :: Double -> Text
showChance = percent commaSF (Just 3)
