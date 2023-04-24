module Sos.Components where

import Components
import Lens.Micro
import Sos.Types
import Sos.Components.Attrs(attributes)

mainComp :: Component BuildIn FullChar
mainComp = Component $
  Entry
    { name = "Charachter"
    , entryLens = id
    , displayLens = id
    , inline = False
    , focusDown = True
    , selectable = True
    , component = Component $
        Menu
          { items =
            [ Entry
              { name = "Race"
              , component = raceComp
              , entryLens = race
              , displayLens = lens (const ()) const
              , inline = True
              , focusDown = False
              , selectable = True
              }
            , Entry
              { name = "Attributes"
              , component = attributes
              , entryLens = attrs
              , displayLens = attrs
              , inline = False
              , focusDown = False
              , selectable = True
              }
            ]
          , focused = 0
          }
    }

raceComp :: Component Race ()
raceComp = Component $ simpleSelector @Race @()
