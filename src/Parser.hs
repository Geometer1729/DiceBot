module Parser (Expr (..), parseExpr) where

import Data.List (foldl)

import Text.Parsec (ParsecT, eof, parse)
import Text.Parsec.Combinator (choice)
import Text.Parsec.Expr (Assoc (..), buildExpressionParser)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token (GenTokenParser (..), TokenParser, makeTokenParser)

import Text.Parsec qualified as Parsec
import Text.Parsec.Expr qualified as Parsec

tokens :: TokenParser t
tokens = makeTokenParser haskellDef

data Expr
  = IntLit Integer
  | Var Text
  | Dice Expr Expr (Maybe Expr)
  | Paren Expr
  | App Expr Expr
  | Infix Text Expr Expr
  | IfTE Expr Expr Expr
  | Lambda Text Expr
  deriving stock (Show, Eq, Ord)

type Parser = forall t. ParsecT String t Identity Expr

parseExpr :: String -> Either Text Expr
parseExpr input =
  parse parser "command" input
    & \case
      Left err -> Left $ show err
      Right res -> Right res

parser :: Parser
parser = expr <* eof

expr :: Parser
expr = specials

specials :: Parser
specials =
  choice
    [ IfTE
        <$> (reserved "if" *> infixes)
        <*> (reserved "then" *> infixes)
        <*> (reserved "else" *> infixes)
    , Lambda
        <$> (reservedOp "\\" *> (toText <$> identifier))
        <*> (reservedOp "->" *> infixes)
    , infixes
    ]
  where
    TokenParser {reserved, identifier, reservedOp} = tokens

infixes :: Parser
infixes = buildExpressionParser table apps
  where
    table =
      [ binary <$> ["++"]
      , binary <$> ["<", "<=", "==", "/=", ">", ">="]
      , binary <$> ["&&", "||"]
      , binary <$> ["^", "**"]
      , binary <$> ["*", "/"]
      , binary <$> ["+", "-"]
      ]
    binary name = Parsec.Infix (reservedOp name $> Infix (toText name)) AssocLeft
    TokenParser {reservedOp} = tokens

apps :: Parser
apps = do
  e <- simple
  rest <- many simple
  case rest of
    [] -> pure e
    es -> pure $ foldl App e es

simple :: Parser
simple =
  choice
    [ parens expr
    , Parsec.try $ Dice <$> intOrParen <*> (Parsec.char 'd' *> intOrParen) <*> (Parsec.char 'k' *> (Just <$> intOrParen))
    , Parsec.try $ Dice (IntLit 1) <$> (Parsec.char 'd' *> intOrParen) <*> pure Nothing
    , Parsec.try $ Dice <$> intOrParen <*> (Parsec.char 'd' *> intOrParen)  <*> pure Nothing
    , intLit
    , Var . toText <$> identifier
    ]
  where
    intOrParen = parens expr <|> intLit
    intLit = IntLit <$> natural
    TokenParser {natural, identifier, parens} = tokens
