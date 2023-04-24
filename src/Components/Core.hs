module Components.Core where

import Graphics.Vty

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

focusAttrs :: Bool -> Attr
focusAttrs cond =
  if cond
    then defAttr `withStyle` bold
    else defAttr
