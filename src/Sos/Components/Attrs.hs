module Sos.Components.Attrs where

import Components
import Lens.Micro
import Sos.Types

attributes :: Component CoreAttrs Attrs
attributes = Component $
  Menu
    { items =
      [ mkAttrDisplay "PCP cost" pcpCost
      , mkAttrDisplay "Points left" freePoints
      , mkAttrBoth "str" str str
      , mkAttrBoth "agi" agi agi
      , mkAttrBoth "end" end end
      , mkAttrBoth "hlt" hlt hlt
      , mkAttrBoth "wil" wil wil
      , mkAttrBoth "wit" wit wit
      , mkAttrBoth "int" int int
      , mkAttrBoth "per" per per
      , mkAttrDisplay "adr" adr
      , mkAttrDisplay "mob" mob
      , mkAttrDisplay "car" car
      , mkAttrDisplay "tou" tou
      , mkAttrDisplay "grt" grt
      ]
      , focused = 2 -- This should really be automatic from the items
    }

mkAttrBoth :: String -> Lens' CoreAttrs Int -> Lens' Attrs Int -> Entry CoreAttrs Attrs
mkAttrBoth name entryLens displayLens =
  Entry
    { focusDown = False
    , inline = True
    , selectable = True
    , component = row
        [Component $ InputInt @Int (Just 0) (Just 8) 0
        ,Component $ Constant @Int @Int " -> "
        ,Component $ Display @Int @Int
        ]
    , ..
    }

mkAttrDisplay :: String -> Lens' Attrs Int -> Entry CoreAttrs Attrs
mkAttrDisplay name displayLens =
  Entry
    { focusDown = False
    , inline = True
    , component = Component $ Display @() @Int
    , entryLens = lens (const ()) const
    , selectable = False
    , ..
    }

