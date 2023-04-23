import Components
import Graphics.Vty
import SosTypes
import Lens.Micro (Lens')

main :: IO ()
main = do
   cfg <- standardIOConfig
   vty <- mkVty cfg
   runComponent vty mainComp
   shutdown vty

mainComp :: Component BuildIn
mainComp = Component $
  Entry
    { name = "Charachter"
    , entryLens = id
    , inline = False
    , focusDown = True
    , component = Component $
        Menu
          { items =
            [ Entry
              { name = "Race"
              , component = raceComp
              , entryLens = bRace
              , inline = True
              , focusDown = False
              }
            , Entry
              { name = "Attributes"
              , component = attributes
              , entryLens = bAttrs
              , inline = False
              , focusDown = False
              }
            ]
          , focused = 0
          }
    }

raceComp :: Component Race
raceComp = Component $ simpleSelector @Race

attributes :: Component CoreAttrs
attributes = Component $
  Menu
    { items =
      [ mkAttrEnt "str" strPoints
      , mkAttrEnt "agi" agiPoints
      , mkAttrEnt "end" endPoints
      , mkAttrEnt "hlt" hltPoints
      , mkAttrEnt "wil" wilPoints
      , mkAttrEnt "wit" witPoints
      , mkAttrEnt "int" intPoints
      , mkAttrEnt "per" perPoints
      ]
      , focused = 0
    }

mkAttrEnt :: String -> Lens' CoreAttrs Int -> Entry CoreAttrs
mkAttrEnt name entryLens =
  Entry
    { focusDown = False
    , inline = True
    , component = Component $ InputInt (Just 0) (Just 8) 0
    , ..
    }
