{-# LANGUAGE TemplateHaskell #-}

module Parser
  (Roll(..)
  ,RollF(..)
  ,RerollOpts(..)
  ,parseRoll
  ) where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Attoparsec.Text
  (Parser
  ,string
  ,decimal
  ,choice
  ,endOfInput
  ,parseOnly
  )

data Roll
  = D RerollOpts Roll Roll
  | C Int
  | Add Roll Roll
  | Mul Roll Roll
  | Sub Roll Roll
  | Div Roll Roll
  deriving stock (Show,Eq,Ord)

data RerollOpts
  = Dont
  | BestOf Int Int
  | WorstOf Int Int
  | UnderMin Int
  | OnceUnderMin Int
  deriving stock (Show,Eq,Ord)

makeBaseFunctor ''Roll

parseRoll :: Text -> Either String Roll
parseRoll = parseOnly roll

roll :: Parser Roll
roll = expr2 <* endOfInput

parens :: Parser Roll
parens = string "(" *> expr2 <* string ")"

constant :: Parser Roll
constant = choice
  [ parens
  , C <$> decimal
  ]

dice :: Parser Roll
dice = choice
  [ (fmap flip . flip) D <$> (constant <|> pure (C 1)) <*> (string "d" *> dice) <*> reroll
  ,constant
  ]

expr1 :: Parser Roll
expr1 = choice
  [ Mul <$> dice <*> (string "*" *> expr1)
  , Div <$> dice <*> (string "/" *> expr1)
  , dice
  ]

expr2 :: Parser Roll
expr2 = choice
  [ Add <$> expr1 <*> (string "+" *> expr2)
  , Sub <$> expr1 <*> (string "-" *> expr2)
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
