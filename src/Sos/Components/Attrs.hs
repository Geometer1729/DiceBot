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
      , mkAttrBoth "str points" str str
      , mkAttrBoth "agi points" agi agi
      , mkAttrBoth "end points" end end
      , mkAttrBoth "hlt points" hlt hlt
      , mkAttrBoth "wil points" wil wil
      , mkAttrBoth "wit points" wit wit
      , mkAttrBoth "int points" int int
      , mkAttrBoth "per points" per per
      , mkAttrDisplay "adr" adr
      , mkAttrDisplay "mob" mob
      , mkAttrDisplay "car" car
      , mkAttrDisplay "tou" tou
      , mkAttrDisplay "grt" grt
      ]
      , focused = 0
    }

mkAttrBoth :: String -> Lens' CoreAttrs Int -> Lens' Attrs Int -> Entry CoreAttrs Attrs
mkAttrBoth name entryLens displayLens =
  Entry
    { focusDown = False
    , inline = True
    , component = Component $
      Join LR
        (Component $ InputInt @Int (Just 0) (Just 8) 0)
        (Component $ Join LR
          (Component $ Constant @Int @Int " -> ")
          (Component $ Display @Int @Int)
        )
    , ..
    }

mkAttrDisplay :: String -> Lens' Attrs Int -> Entry CoreAttrs Attrs
mkAttrDisplay name displayLens =
  Entry
    { focusDown = False
    , inline = True
    , component = Component $ Display @() @Int
    , entryLens = lens (const ()) const
    , ..
    }

mkAttrEnt :: String -> Lens' CoreAttrs Int -> Entry CoreAttrs Attrs
mkAttrEnt name entryLens =
  Entry
    { focusDown = False
    , inline = True
    , component = Component $ InputInt @() (Just 0) (Just 8) 0
    , displayLens = lens (const ()) const
    , ..
    }

