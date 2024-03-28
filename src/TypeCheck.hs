module TypeCheck where

import Parser (Expr, parseExpr)
import Parser qualified as P

import DicePrelude (prelude)
import TypeCheckerCore

import Data.Map qualified as M
import Data.Singletons (
  SingInstance (SingInstance),
  demote,
  fromSing,
  sing,
  singInstance,
 )
import Data.Singletons.Decide (decideEquality, type (:~:) (Refl))

type Roll = ExprT DInt

testEval :: Res -> String
testEval (Res (e :: ExprT d)) =
  case sing @d of
    SDInt -> show $ eval e
    _ -> "Test eval requires an int at top level"
  where
    eval :: ExprT (t :: DType) -> HsOf t
    eval (Lit l) = l
    eval (App f x) = eval f $ eval x
    eval (Dice _ _) = error "Test eval doesn't roll dice"

parseAndType :: Text -> Either Text Roll
parseAndType expr = do
  r <- parseExpr $ toString expr
  typeTopLevel r

typeTopLevel :: Expr -> Either Text (ExprT DInt)
typeTopLevel e = case evalStateT (typeExpr e) 0 of
  Left err -> Left err
  Right (Res (t :: ExprT d)) -> case sing @d of
    SDInt -> Right t
    bad -> Left $ "Top level dice expresions must be of type Int your expresion was of type " <> show (fromSing bad)

typeExpr :: Expr -> StateT Natural (Either Text) Res
typeExpr (P.IntLit n) = lift $ Right $ Res @DInt (Lit $ fromInteger n)
typeExpr (P.App f x) = do
  Res (fe :: ExprT ft) <- typeExpr f
  Res (xe :: ExprT xt) <- typeExpr x
  case sing @ft of
    SDFun @xt' @yt xt yt -> case decideEquality xt (sing @xt) of
      Nothing -> lift $ Left $ "Couldn't match " <> show (demote @xt) <> " with " <> show (fromSing xt)
      Just (Refl :: xt' :~: xt) ->
        -- Case brings SingI yt into scope
        case singInstance yt of
          SingInstance -> pure $ Res $ App fe xe
    _ -> lift $ Left "Applied argument to non-function. too many arguments?"
typeExpr (P.Paren e) = typeExpr e
typeExpr (P.Infix name e1 e2) = typeExpr $ P.App (P.App (P.Var name) e1) e2
typeExpr (P.IfTE e1 e2 e3) = typeExpr $ P.App (P.App (P.App (P.Var "ifte") e1) e2) e3
typeExpr (P.Var name) =
  case M.lookup name prelude of
    Just res -> pure res
    Nothing -> lift $ Left $ "Not in scope " <> name
typeExpr (P.Dice l r) = do
  Res (lt :: ExprT lt) <- typeExpr l
  Res (rt :: ExprT rt) <- typeExpr r
  case (sing @lt, sing @rt) of
    (SDInt, SDInt) -> pure $ Res $ Dice lt rt
    (SDInt, _) -> lift $ Left "number of faces must have type Int"
    (_, _) -> lift $ Left "number of dice must have type Int"
typeExpr (P.Lambda _ _) = lift $ Left "Lambda functions are not implemented yet"
