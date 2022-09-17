module RefTable (
  RefTable,
  newRefTable,
  maybeMakeRef,
  maybeUnRef,
) where

import Data.Map as M
import Data.Text as T
import Data.Hashable as H

import Control.Concurrent.STM.TVar (stateTVar)

type RefTable = TVar (Map Int Text)

newRefTable :: MonadIO m => m RefTable
newRefTable = newTVarIO mempty

addLM :: MonadIO m => RefTable -> Text -> m Int
addLM lm t =
  atomically $
    stateTVar lm $
      \m -> let h = H.hash t in (h, M.insert h t m)

lookupLM :: MonadIO m => RefTable -> Int -> m (Maybe Text)
lookupLM lm n = do
  m <- readTVarIO lm
  pure $ M.lookup n m

maybeMakeRef :: MonadIO m => RefTable -> Text -> m Text
maybeMakeRef lm txt
  | T.length txt <= 100 = pure txt
  | T.length txt <= 2000 = do
    ref <- addLM lm txt
    pure $ "ref:" <> show ref
  | otherwise = pure "sorry content was over 2000 charachters"

maybeUnRef :: MonadIO m => RefTable -> Text -> m Text
maybeUnRef lm = \case
  (stripPrefix "ref:" -> Just rest) ->
    case readMaybe $ toString rest of
      Nothing -> die "failed to read a logref"
      Just ref ->
        lookupLM lm ref >>= \case
          Nothing -> pure "ref:lost"
          Just txt' -> pure txt'
  txt -> pure txt
