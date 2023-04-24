module Components.Menus where

import Components.Core
import Lens.Micro
import Graphics.Vty
import Data.Default

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
      (string (focusAttrs active) (name <> ":"))
      (render (active && focusDown) component (o ^. displayLens))

  upd Entry{component,entryLens} =
    over entryLens (upd component)

data Menu i o =
  Menu
    { items :: [Entry i o]
    , focused :: Int
    }

instance Interface (Menu i o) i o where
  -- AFAICT the existential types prevent cleaning this up with lenses
  handle m@Menu{items,focused} event = let
    item = (fromMaybe (error "bad menu index") $ items !!? focused)
    defferToItem =
      case handle item event of
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

