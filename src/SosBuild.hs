module SosBuild where

import SosTypes

buildAttrs :: Race -> CoreAttrs -> Attrs
buildAttrs race CoreAttrs{..} = let
   whenDwarf n = if race == Dwarf then n else 0
   whenGob n = if race == Goblin then n else 0
   str = 1 + _strPoints - whenGob 2
   agi = 1 + _agiPoints + whenGob 1
   end = 1 + _endPoints + whenDwarf 2
   hlt = 1 + _hltPoints + whenDwarf 1
   wil = 1 + _wilPoints
   wit = 1 + _witPoints
   int = 1 + _intPoints
   per = 1 + _perPoints + whenGob 1
   adr = (agi + wit) `div` 2
   mob = (str + agi + end) `div` 2 - whenDwarf 2
   car = str + end
   cha = (wil + per + wit) `div` 2
   tou = 4 + whenDwarf 1
   grt = wil `div` 2
   attrSpent =
     sum
     [_strPoints
     ,_agiPoints
     ,_endPoints
     ,_hltPoints
     ,_wilPoints
     ,_witPoints
     ,_intPoints
     ,_perPoints
     ]
   levels = [22,23,24,27,31,35,40,45,50,56]
   (pcpCost,attrPoints) = fromTable levels attrSpent
   freePoints = attrPoints - attrSpent
   in Attrs{..}

fromTable :: Ord a => [a] -> a -> (Int,a)
fromTable t v =
  fromMaybe (error "table ran out")
  $ find (\(_,c) -> v <= c) (zip [1..] t)
