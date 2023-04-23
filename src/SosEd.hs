import Components
import Graphics.Vty
import SosTypes
import Lens.Micro (Lens',lens)
import SosBuild (buildChar, defChar)

main :: IO ()
main = do
   cfg <- standardIOConfig
   vty <- mkVty cfg
   runComponent vty buildChar defChar mainComp
   shutdown vty

mainComp :: Component BuildIn FullChar
mainComp = Component $
  Entry
    { name = "Charachter"
    , entryLens = id
    , displayLens = id
    , inline = False
    , focusDown = True
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
              }
            , Entry
              { name = "Attributes"
              , component = attributes
              , entryLens = attrs
              , displayLens = attrs
              , inline = False
              , focusDown = False
              }
            ]
          , focused = 0
          }
    }

raceComp :: Component Race ()
raceComp = Component $ simpleSelector @Race

attributes :: Component CoreAttrs Attrs
attributes = Component $
  Menu
    { items =
      [ mkAttrDisplay "PCP cost" pcpCost
      , mkAttrDisplay "Points left" freePoints
      , mkAttrEnt "str points" str
      , mkAttrEnt "agi points" agi
      , mkAttrEnt "end points" end
      , mkAttrEnt "hlt points" hlt
      , mkAttrEnt "wil points" wil
      , mkAttrEnt "wit points" wit
      , mkAttrEnt "int points" int
      , mkAttrEnt "per points" per
      , mkAttrDisplay "str" str
      , mkAttrDisplay "agi" agi
      , mkAttrDisplay "end" end
      , mkAttrDisplay "hlt" hlt
      , mkAttrDisplay "wil" wil
      , mkAttrDisplay "wit" wit
      , mkAttrDisplay "int" int
      , mkAttrDisplay "per" per
      , mkAttrDisplay "adr" adr
      , mkAttrDisplay "mob" mob
      , mkAttrDisplay "car" car
      , mkAttrDisplay "tou" tou
      , mkAttrDisplay "grt" grt
      ]
      , focused = 0
    }

mkAttrDisplay :: String -> Lens' Attrs Int -> Entry CoreAttrs Attrs
mkAttrDisplay name displayLens =
  Entry
    { focusDown = False
    , inline = True
    , component = Component (Display @Int)
    , entryLens = lens (const ()) const
    , ..
    }

mkAttrEnt :: String -> Lens' CoreAttrs Int -> Entry CoreAttrs Attrs
mkAttrEnt name entryLens =
  Entry
    { focusDown = False
    , inline = True
    , component = Component $ InputInt (Just 0) (Just 8) 0
    , displayLens = lens (const ()) const
    , ..
    }
