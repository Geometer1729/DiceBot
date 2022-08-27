module RefTable
  (RefTable
  ,newRefTable
  ,maybeMakeRef
  ,maybeUnRef
  )
  where

import Data.Map as M
import Data.Text as T

import Control.Concurrent.STM.TVar(stateTVar)

type RefTable = TVar (Int,Map Int Text)

newRefTable :: MonadIO m => m RefTable
newRefTable = newTVarIO (0,mempty)

addLM :: MonadIO m => RefTable -> Text -> m Int
addLM lm t =
  atomically $
  stateTVar lm $
    \(n,m) -> (n,(n+1,M.insert n t m))

lookupLM :: MonadIO m => RefTable -> Int -> m (Maybe Text)
lookupLM lm n = do
  (_,m) <- readTVarIO lm
  pure $ M.lookup n m

maybeMakeRef :: MonadIO m =>RefTable -> Int -> Text -> m Text
maybeMakeRef lm space txt
  | T.length txt <= space = pure txt
  | T.length txt <= 2000 = do
     ref <- addLM lm txt
     pure $ "ref:" <> show ref
  | otherwise = pure "content was over 2000 charachters"

maybeUnRef :: MonadIO m => RefTable -> Text -> m Text
maybeUnRef lm = \case
  (stripPrefix "ref:" -> Just rest) ->
       case readMaybe $ toString rest of
         Nothing -> die "failed to read a logref"
         Just ref ->
           lookupLM lm ref >>= \case
             Nothing -> die "failed to lookup ref"
             Just txt' -> pure txt'
  txt -> pure txt

