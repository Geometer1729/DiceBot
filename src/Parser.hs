{-# LANGUAGE TemplateHaskell #-}

module Parser (
  Roll (..),
  RollF (..),
  RerollOpts (..),
  RerollBest (..),
  Dir (..),
  RerollUnder (..),
  parseRoll,
  -- debug
  roll,
  reroll,
  rerollUnder,
  rerollBest,
) where

import Data.Attoparsec.Text (
  Parser,
  choice,
  decimal,
  endOfInput,
  parseOnly,
  signed,
  skipSpace,
  string,
 )
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Text.Show qualified as Show

data Roll
  = D RerollOpts Roll Roll
  | C Int
  | Add Roll Roll
  | Mul Roll Roll
  | Sub Roll Roll
  | Div Roll Roll
  deriving stock (Show, Eq, Ord)

data RerollOpts = RerollOpts
  { best :: Maybe RerollBest
  , under :: Maybe RerollUnder
  }
  deriving stock (Eq, Ord)

instance Show RerollOpts where
  show RerollOpts {best, under} =
    case fold (show <$> best) <> fold (show <$> under) of
      "" -> ""
      opts -> " reroll " <> opts

data RerollBest = RerollBest {dir :: Dir, amt :: Int, keep :: Int}
  deriving stock (Eq, Ord)

data Dir = Best | Worst
  deriving stock (Eq, Ord)

instance Show RerollBest where
  show = \case
    RerollBest Best a b -> show a <> " keep best " <> show b
    RerollBest Worst a b -> show a <> " keep worst " <> show b

data RerollUnder
  = Under Int
  | OnceUnder Int
  deriving stock (Eq, Ord)

instance Show RerollUnder where
  show = \case
    Under n -> "up to " <> show n
    OnceUnder n -> "once up to " <> show n

makeBaseFunctor ''Roll

parseRoll :: Text -> Either String Roll
parseRoll = parseOnly roll

roll :: Parser Roll
roll = expr2 <* endOfInput

parens :: Parser Roll
parens = string "(" *> expr2 <* string ")"

constant :: Parser Roll
constant =
  choice
    [ parens
    , C <$> signed decimal
    ]

dice :: Parser Roll
dice =
  choice
    [ (fmap flip . flip) D <$> (constant <|> pure (C 1)) <*> (string "d" *> dice) <*> reroll
    , constant
    ]

expr1 :: Parser Roll
expr1 =
  choice
    [ Mul <$> dice <*> (string "*" *> expr1)
    , Div <$> dice <*> (string "/" *> expr1)
    , dice
    ]

expr2 :: Parser Roll
expr2 =
  choice
    [ Add <$> expr1 <*> (string "+" *> expr2)
    , Sub <$> expr1 <*> (string "-" *> expr2)
    , expr1
    ]

reroll :: Parser RerollOpts
reroll =
  choice
    [ (skipSpace *> (string "reroll" <|> string "r") *> skipSpace) *> (RerollOpts <$> rerollBest <*> rerollUnder)
    , pure $ RerollOpts Nothing Nothing
    ]

rerollBest :: Parser (Maybe RerollBest)
rerollBest =
  choice
    [ (Just <$>) $
        signed decimal
          <**> ( skipSpace >> RerollBest
                  <$> ( (string "keep best" $> Best)
                          <|> (string "keep worst" $> Worst)
                          <|> (string "keep" $> Best)
                          <|> (string "kb" $> Best)
                          <|> (string "kw" $> Worst)
                          <|> (string "k" $> Best)
                      )
                    <* skipSpace
               )
          <*> signed decimal
    , pure Nothing
    ]

rerollUnder :: Parser (Maybe RerollUnder)
rerollUnder =
  choice
    [ (Just <$>) $
        ( (skipSpace *> (string "once up to" <|> string "ou") *> skipSpace $> OnceUnder)
            <|> (skipSpace *> (string "up to" <|> string "u") *> skipSpace $> Under)
        )
          <*> signed decimal
    , pure Nothing
    ]