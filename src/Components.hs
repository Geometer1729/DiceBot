module Components where

import Prelude hiding((<|>))
import Graphics.Vty
import Lens.Micro

runComponent :: Vty -> Component a -> IO ()
runComponent vty component = do
  update vty (picForImage $ render True component)
  e <- nextEvent vty
  case e of
    EvKey (KChar 'q') _ -> pass
    _ -> runComponent vty (fromMaybe component $ handle e component)
--
-- TODO should I just combine these classes?

class Handler a where
  handle :: Event -> a -> Maybe a
  -- Nothing indicates moving focus up the tree

class Render a where
  render :: Bool -> a -> Image

class Models c v | c -> v where
  upd :: c -> v -> v

data Component b where
  Component :: (Handler a,Render a,Models a b) => a -> Component b

instance Handler (Component a) where
  handle e (Component c) = Component <$> handle e c

instance Render (Component a) where
  render b (Component c) = render b c

instance Models (Component a) a where
  upd (Component c) = upd c

data InputInt =
  InputInt
    { minVal :: Maybe Int
    , maxVal :: Maybe Int
    , value :: Int
    }

instance Render InputInt where
  render active InputInt{value} =
    string (boldIf active) (show value)

instance Handler InputInt where
  handle = \case
    EvKey (KChar 'a') _ -> \c@InputInt{maxVal,value}
      -> Just c{value=maybe id min maxVal $ value + 1}
    EvKey (KChar 'x') _ -> \c@InputInt{minVal,value}
      -> Just c{value=maybe id max minVal $ value - 1}
    EvKey (KChar 'h') _ -> const Nothing
    _ -> pure

instance Models InputInt Int where
  upd i = const $ value i

data Entry b = forall a.
  Entry
    { component :: Component a
    , name :: String
    , entryLens :: Lens' b a
    , inline :: Bool
    , focusDown :: Bool
    }

instance Render (Entry b) where
  render active Entry{name,component,inline,focusDown}
    = (if inline then horizJoin else vertJoin)
      (string (boldIf active) (name <> ":"))
      (render (active && focusDown) component)

instance Handler (Entry b) where
  handle event Entry{..} =
    handle event component <&> \c ->
      Entry {component = c,..}
    -- GHC doesn't like record updates here

instance Models (Entry b) b where
  upd Entry{component,entryLens} =
    over entryLens (upd component)

data Menu a =
  Menu
    { items :: [Entry a]
    , focused :: Int
    }

instance Models (Menu a) a where
  upd Menu{items} m = flipfoldl' upd m items

instance Render (Menu a) where
  render active Menu{items,focused}
    = mconcat
      (zip [0..] items <&>
        (\(ind,item) ->
          pad 2 0 0 0 $ -- indent
          render (active && focused == ind) item
        )
      )

instance Handler (Menu a) where
  handle event m@Menu{items,focused} = let
    item = (fromMaybe (error "bad menu index") $ items !!? focused)
    defferToItem =
      case handle event item of
            Nothing -> Just m{items = items & ix focused .~ item{focusDown = False}}
            Just item' -> Just m{items= items & ix focused .~ item'}
    in if focusDown item
      then defferToItem
      else case event of
        EvKey (KChar 'j') _ -> Just m{focused = min (length items -1) (focused + 1)}
        EvKey (KChar 'k') _ -> Just m{focused = max 0 (focused -1)}
        EvKey (KChar 'l') _ -> Just m{items = items & ix focused .~ item{focusDown = True}}
        EvKey (KChar 'h') _ -> Nothing
        _ -> if inline item then defferToItem else Just m

data Selector a =
  Selector
    { ind :: Int
    , ents :: [(String,a)]
    }

selected :: Selector a -> (String,a)
selected Selector{ind,ents} =
  fromMaybe (error "invalid selector index")
    $ ents !!? ind

instance Models (Selector a) a where
  upd = const . snd . selected

instance Render (Selector a) where
  render active = string (boldIf active) . fst . selected

instance Handler (Selector a) where
  handle event s@Selector{ind,ents} =
    case event of
      EvKey (KChar 'j') _ -> Just s{ind = min (length ents -1) (ind + 1)}
      EvKey (KChar 'k') _ -> Just s{ind = max 0 (ind -1)}
      EvKey (KChar 'h') _ -> Nothing
      _ -> Just s

simpleSelector :: (Enum a,Bounded a,Show a) => Selector a
simpleSelector = Selector
  { ind = 0
  , ents = [(show a,a) | a <- universe ]
  }

-- utils
boldIf :: Bool -> Attr
boldIf cond = if cond then defAttr `withStyle` bold else defAttr
