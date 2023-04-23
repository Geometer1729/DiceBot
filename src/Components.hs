{-# LANGUAGE ScopedTypeVariables #-}

module Components where

import Graphics.Vty
import Lens.Micro

runComponent :: Vty -> (i -> o) -> i -> Component i o -> IO ()
runComponent vty builder inp component = do
  update vty (picForImage $ render True component (builder inp))
  e <- nextEvent vty
  case e of
    EvKey (KChar 'q') _ -> pass
    _ -> case handle component e of
            Right new -> runComponent vty builder (upd new inp) new
            _ -> runComponent vty builder inp component

class Interface c i o | c -> i , c -> o where
  handle :: c -> Event -> Either InterfaceAction c
  render :: Bool -> c -> o -> Image
  upd :: c -> i -> i

data InterfaceAction = FocusBack | Pass

data Component i o where
  Component :: Interface c i o => c -> Component i o

instance Interface (Component i o) i o where
  handle (Component c) e = Component <$> handle c e
  render b (Component c) = render b c
  upd (Component c) = upd c
    -- sets coerce is the trivial setter for the component newtype

data InputInt =
  InputInt
    { minVal :: Maybe Int
    , maxVal :: Maybe Int
    , value :: Int
    }

instance Interface InputInt Int () where
  handle c@InputInt{maxVal,minVal,value} = \case
    EvKey (KChar 'a') _ ->
      pure c{value=maybe id min maxVal $ value + 1}
    EvKey (KChar 'x') _ ->
      pure c{value=maybe id max minVal $ value - 1}
    EvKey (KChar 'h') _ -> Left FocusBack
    _ -> Left Pass

  render active InputInt{value} () =
    string (boldIf active) (show value)

  upd InputInt{value} _ = value

data Entry i o = forall ic oc.
  Entry
    { component :: Component ic oc
    , name :: String
    , entryLens :: Lens' i ic
    , displayLens :: Lens' o oc
    , inline :: Bool
    , focusDown :: Bool
    }

instance Interface (Entry i o) i o where
  handle Entry{..} event =
    handle component event <&> \c ->
      Entry {component = c,..}
    -- GHC doesn't like record updates here

  render active Entry{name,component,inline,focusDown,displayLens} o
    = (if inline then horizJoin else vertJoin)
      (string (boldIf active) (name <> ":"))
      (render (active && focusDown) component (o ^. displayLens))

  upd Entry{component,entryLens} =
    over entryLens (upd component)

data Menu i o =
  Menu
    { items :: [Entry i o]
    , focused :: Int
    }

instance Interface (Menu i o) i o where
  handle m@Menu{items,focused} event = let
    item = (fromMaybe (error "bad menu index") $ items !!? focused)
    defferToItem =
      case handle item event of
          -- AFAICT the existential types prevent cleaning this up with lenses
          Left FocusBack -> pure m{items = items & ix focused .~ item{focusDown = False}}
          Left Pass -> Left Pass
          Right item' -> pure m{items= items & ix focused .~ item'}
    in if focusDown item
      then defferToItem
      else case event of
        EvKey (KChar 'j') _ -> pure m{focused = min (length items -1) (focused + 1)}
        EvKey (KChar 'k') _ -> pure m{focused = max 0 (focused -1)}
        EvKey (KChar 'l') _ -> pure m{items = items & ix focused .~ item{focusDown = True}}
        EvKey (KChar 'h') _ -> Left FocusBack
        _ -> if inline item then defferToItem else Left Pass

  render active Menu{items,focused} o
    = mconcat
      (zip [0..] items <&>
        (\(ind,item) ->
          pad 2 0 0 0 $ -- indent
          render (active && focused == ind) item o
        )
      )

  upd Menu{items} m = flipfoldl' upd m items

data Selector a =
  Selector
    { ind :: Int
    , ents :: [(String,a)]
    }

selected :: Selector a -> (String,a)
selected Selector{ind,ents} =
  fromMaybe (error "invalid selector index")
    $ ents !!? ind

instance Interface (Selector i) i () where
  handle s@Selector{ind,ents} event =
    case event of
      EvKey (KChar 'j') _ -> pure s{ind = min (length ents -1) (ind + 1)}
      EvKey (KChar 'k') _ -> pure s{ind = max 0 (ind -1)}
      EvKey (KChar 'h') _ -> Left FocusBack
      _ -> Left Pass

  render active = flip $ const $ string (boldIf active) . fst . selected

  upd = const . snd . selected

simpleSelector :: (Enum a,Bounded a,Show a) => Selector a
simpleSelector = Selector
  { ind = 0
  , ents = [(show a,a) | a <- universe ]
  }

data Display a = Display

instance Show a => Interface (Display a) () a where
  handle _ event =
    case event of
      EvKey (KChar 'h') _ -> Left FocusBack
      _ -> Left Pass

  render active Display o = string (boldIf active) (show o)

  upd _ _ = ()

-- Utils

boldIf :: Bool -> Attr
boldIf cond = if cond then defAttr `withStyle` bold else defAttr

