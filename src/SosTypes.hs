{-# LANGUAGE TemplateHaskell #-}

module SosTypes where

import Lens.Micro.TH(makeLenses)

data CoreAttrs =
  CoreAttrs -- must be between 0 and 8
    { _strPoints :: Int
    , _agiPoints :: Int
    , _endPoints :: Int
    , _hltPoints :: Int
    , _wilPoints :: Int
    , _witPoints :: Int
    , _intPoints :: Int
    , _perPoints :: Int
    }

makeLenses ''CoreAttrs

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
  { str :: Int
  , agi :: Int
  , end :: Int
  , hlt :: Int
  , wil :: Int
  , wit :: Int
  , int :: Int
  , per :: Int
  , adr :: Int
  , mob :: Int
  , car :: Int
  , cha :: Int
  , tou :: Int
  , grt :: Int
  , attrPoints :: Int
  , freePoints :: Int
  , pcpCost :: Int
  }


data BuildIn = BuildIn
  { _bRace :: Race
  , _bAttrs :: CoreAttrs
  , _bSkills :: Skills
  , _bGear :: [Item]
  , _bProfs :: Profs
  , _bBoons :: Boons
  }

makeLenses ''BuildIn

data FullChar = FullChar
  { _race :: Race
  , _attrs :: Attrs
  , _gear :: [Item]
  }

