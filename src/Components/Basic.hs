module Components.Basic where

import Components.Core
import Graphics.Vty

data InputInt o =
  InputInt
    { minVal :: Maybe Int
    , maxVal :: Maybe Int
    , value :: Int
    }

instance Interface (InputInt o) Int o where
  handle c@InputInt{maxVal,minVal,value} = \case
    EvKey (KChar 'a') _ ->
      pure c{value=maybe id min maxVal $ value + 1}
    EvKey (KChar 'x') _ ->
      pure c{value=maybe id max minVal $ value - 1}
    EvKey (KChar 'h') _ -> Left FocusBack
    _ -> Left Pass

  render active InputInt{value} _ =
    string (focusAttrs active) (show value)

  upd InputInt{value} _ = value

data Selector i o =
  Selector
    { ind :: Int
    , ents :: [(String,i)]
    }

selected :: Selector i o -> (String,i)
selected Selector{ind,ents} =
  fromMaybe (error "invalid selector index")
    $ ents !!? ind

instance Interface (Selector i o) i o where
  handle s@Selector{ind,ents} event =
    case event of
      EvKey (KChar 'j') _ -> pure s{ind = min (length ents -1) (ind + 1)}
      EvKey (KChar 'k') _ -> pure s{ind = max 0 (ind -1)}
      EvKey (KChar 'h') _ -> Left FocusBack
      _ -> Left Pass

  render active = flip $ const $ string (focusAttrs active) . fst . selected

  upd = const . snd . selected

simpleSelector :: (Enum i,Bounded i,Show i) => Selector i o
simpleSelector = Selector
  { ind = 0
  , ents = [(show a,a) | a <- universe ]
  }

data Display i o = Display

instance Show o => Interface (Display i o) i o where
  handle _ event =
    case event of
      EvKey (KChar 'h') _ -> Left FocusBack
      _ -> Left Pass

  render active Display o = string (focusAttrs active) (show o)

  upd _ = id

newtype Constant i o = Constant String

instance Interface (Constant i o) i o where
  handle _ _ = Left Pass
  render  active (Constant a) _ = string (focusAttrs active) a
  upd _ = id
