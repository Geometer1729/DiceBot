{-# LANGUAGE TemplateHaskell #-}

module SosTypes where

import Lens.Micro.TH(makeLensesWith,camelCaseFields)

data CoreAttrs =
  CoreAttrs -- must be between 0 and 8
    { coreAttrsStr :: Int
    , coreAttrsAgi :: Int
    , coreAttrsEnd :: Int
    , coreAttrsHlt :: Int
    , coreAttrsWil :: Int
    , coreAttrsWit :: Int
    , coreAttrsInt :: Int
    , coreAttrsPer :: Int
    }

makeLensesWith camelCaseFields ''CoreAttrs

data Race
  = Human
  | Goblin
  | Dwarf
  deriving stock (Eq,Ord,Show,Enum,Bounded)

data Skills = Skills
  -- TODO implement skills

data Profs = Profs

newtype Boons = Boons{getBoons :: [(Int,String)]}
  -- for now boons only tack cost

data Item
  = Item
    { name :: String
    , wt :: Double
    , cost :: Cost
    , use :: Use
    }

newtype Cost = Cost Int
  deriving newtype (Num,Eq,Ord)
  -- In Copper
cp,sp,gp :: Cost
cp = 1
sp = 12
gp = 20 * sp

data Use
  = Weapon Weapon
  | Armor Armor

data Weapon where

data Armor where

data Attrs = Attrs
  { attrsStr :: Int
  , attrsAgi :: Int
  , attrsEnd :: Int
  , attrsHlt :: Int
  , attrsWil :: Int
  , attrsWit :: Int
  , attrsInt :: Int
  , attrsPer :: Int
  , attrsAdr :: Int
  , attrsMob :: Int
  , attrsCar :: Int
  , attrsCha :: Int
  , attrsTou :: Int
  , attrsGrt :: Int
  , attrsAttrPoints :: Int
  , attrsFreePoints :: Int
  , attrsPcpCost :: Int
  }

makeLensesWith camelCaseFields ''Attrs


data BuildIn = BuildIn
  { buildInRace :: Race
  , buildInAttrs :: CoreAttrs
  , buildInSkills :: Skills
  , buildInGear :: [Item]
  , buildInProfs :: Profs
  , buildInBoons :: Boons
  }

makeLensesWith camelCaseFields ''BuildIn

data FullChar = FullChar
  { fullCharRace :: Race
  , fullCharAttrs :: Attrs
  , fullCharGear :: [Item]
  }

makeLensesWith camelCaseFields ''FullChar
