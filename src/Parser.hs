{-# LANGUAGE UndecidableInstances #-}

module Parser
  (Roll
  ,Fix(..)
  ,RollF(..)
  ,RerollOpts(..)
  ,parseRoll
  ) where

import Data.Attoparsec.Text
  (Parser
  ,string
  ,decimal
  ,choice
  ,endOfInput
  ,parseOnly
  )

type Roll = Fix RollF
newtype Fix f = InF { outF :: f (Fix f) }

deriving stock instance Show (f (Fix f)) => Show (Fix f)
deriving stock instance Eq (f (Fix f)) => Eq (Fix f)
deriving stock instance Ord (f (Fix f)) => Ord (Fix f)

data RollF r
  = D RerollOpts r r
  | C Int
  | Add r r
  | Mul r r
  | Sub r r
  | Div r r
  deriving stock (Show,Eq,Ord,Functor)

data RerollOpts
  = Dont
  | BestOf Int Int
  | WorstOf Int Int
  | UnderMin Int
  | OnceUnderMin Int
  deriving stock (Show,Eq,Ord)

parseRoll :: Text -> Either String Roll
parseRoll = parseOnly roll

roll :: Parser Roll
roll = expr2 <* endOfInput

parens :: Parser Roll
parens = string "(" *> expr2 <* string ")"

constant :: Parser Roll
constant = choice
  [ parens
  , InF . C <$> decimal
  ]

dice :: Parser Roll
dice = choice
  [ fmap InF $ (fmap flip . flip) D <$> (constant <|> pure (InF $ C 1)) <*> (string "d" *> dice) <*> reroll
  ,constant
  ]

expr1 :: Parser Roll
expr1 = choice
  [ fmap InF $ Mul <$> dice <*> (string "*" *> expr1)
  , fmap InF $ Div <$> dice <*> (string "/" *> expr1)
  , dice
  ]

expr2 :: Parser Roll
expr2 = choice
  [ fmap InF $ Add <$> expr1 <*> (string "+" *> expr2)
  , fmap InF $ Sub <$> expr1 <*> (string "-" *> expr2)
  , expr1
  ]

reroll :: Parser RerollOpts
reroll = choice
  [ BestOf <$> (string "r" *> decimal) <*> ((string "k" <|> string "kb") *> decimal <|> pure 1) -- reroll _ keep (best) _
  , WorstOf <$> (string "r" *> decimal) <*> (string "kw" *> decimal <|> pure 1) -- reroll _ keep worst _
  , UnderMin <$> (string "ru" *> decimal) -- up to
  , OnceUnderMin <$> (string "rou" *> decimal) -- once up to
  , pure Dont
  ]
