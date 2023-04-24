module Sos.Build where

import Sos.Types
import Lens.Micro ((^.))

buildChar :: BuildIn -> FullChar
buildChar b =
  FullChar
    { fullCharRace = b ^. race
    , fullCharAttrs = buildAttrs (b ^. race) (b ^. attrs)
    , fullCharGear = []
    }

defChar :: BuildIn
defChar = BuildIn
  { buildInRace = Human
  , buildInAttrs = CoreAttrs 0 0 0 0 0 0 0 0
  , buildInSkills = Skills
  , buildInGear = []
  , buildInProfs = Profs
  , buildInBoons = Boons []
  }

buildAttrs :: Race -> CoreAttrs -> Attrs
buildAttrs r c = let
   whenDwarf n = if r == Dwarf then n else 0
   whenGob n = if r == Goblin then n else 0
   attrsStr = 1 + c^.str - whenGob 2
   attrsAgi = 1 + c^.agi + whenGob 1
   attrsEnd = 1 + c^.end + whenDwarf 2
   attrsHlt = 1 + c^.hlt + whenDwarf 1
   attrsWil = 1 + c^.wil
   attrsWit = 1 + c^.wit
   attrsInt = 1 + c^.int
   attrsPer = 1 + c^.per + whenGob 1
   attrsAdr = (c^.agi + c^.wit) `div` 2
   attrsMob = (c^.wit + c^.agi + c^.end) `div` 2 - whenDwarf 2
   attrsCar = c^.str + c^.end
   attrsCha = (c^.wil + c^.per + c^.wit) `div` 2
   attrsTou = 4 + whenDwarf 1
   attrsGrt = c^.wil `div` 2
   attrsAttrSpent =
     sum
     [c^.str
     ,c^.agi
     ,c^.end
     ,c^.hlt
     ,c^.wil
     ,c^.wit
     ,c^.int
     ,c^.per
     ]
   levels = [22,23,24,27,31,35,40,45,50,56]
   (attrsPcpCost,attrsAttrPoints) = fromTable levels attrsAttrSpent
   attrsFreePoints = attrsAttrPoints - attrsAttrSpent
   in Attrs{..}

fromTable :: Ord a => [a] -> a -> (Int,a)
fromTable t v =
  fromMaybe (error "table ran out")
  $ find (\(_,c) -> v <= c) (zip [1..] t)
