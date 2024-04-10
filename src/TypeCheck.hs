module TypeCheck where

import Parser (Expr, parseExpr)
import Parser qualified as P

import DicePrelude (prelude)
import TypeCheckerCore

import Data.Map qualified as M
import Data.Singletons.Decide (decideEquality, type (:~:) (Refl))
import Prelude.Singletons
import Cast (cast)

type Roll = ExprT DInt

parseAndType :: Text -> Either Text Roll
parseAndType expr = do
  r <- parseExpr $ toString expr
  typeTopLevel r

typeTopLevel :: Expr -> Either Text (ExprT DInt)
typeTopLevel e = case evalStateT (typeExpr e) (ScopeNat $ sing @0) of
  Left err -> Left err
  Right (Res (t :: ExprT d)) -> case sing @d of
    SDInt -> Right t
    bad -> Left $ "Top level dice expresions must be of type `Int` your expresion was of type `" <> pShow (fromSing bad) <> "`"

data ScopeNat where
  ScopeNat :: forall (n :: Natural) . Sing n -> ScopeNat

typeExpr :: Expr -> StateT ScopeNat (Either Text) Res
typeExpr (P.IntLit n) = pure $ cast $ fromInteger @Int n
typeExpr (P.Paren e) = typeExpr e
typeExpr (P.Infix name e1 e2) = typeExpr $ P.App (P.App (P.Var name) e1) e2
typeExpr (P.IfTE e1 e2 e3) = typeExpr $ P.App (P.App (P.App (P.Var "ifte") e1) e2) e3
typeExpr (P.Dice l r) = do
  Res (lt :: ExprT lt) <- typeExpr l
  Res (rt :: ExprT rt) <- typeExpr r
  case (sing @lt, sing @rt) of
    (SDInt, SDInt) -> pure $ Res $ Dice lt rt
    (SDInt, _) -> lift $ Left "number of faces must have type Int"
    (_, _) -> lift $ Left "number of dice must have type Int"
typeExpr (P.Var name) =
  case M.lookup name prelude of
    Just (Res (res :: ExprT d)) -> do
      -- Whenever a polymorphic term is introduced it needs to be refined
      -- to not use any existing type vars and the state needs to be updated
      ScopeNat start <- get
      let new = sFreeVar (sing @d)
      put $ ScopeNat (start %+ new)
      let refs = sReScopeRefs start
          res' = case refs of
            (_ :: Sing refs) -> withSingI refs $ refineExpr @refs res
      withSingI (sRefine refs (sing @d)) $
        pure $ Res res'

    Nothing -> lift $ Left $ "Not in scope " <> name
typeExpr (P.Lambda _ _) = lift $ Left "Lambda functions are not implemented yet"
typeExpr (P.App f x) = do
  Res (fe :: ExprT ft) <- typeExpr f
  Res (xe :: ExprT xt) <- typeExpr x
  case sing @ft of
    SDFun @xt' @yt xt' yt -> withSingI yt $
      case decideEquality xt' (sing @xt) of
        Just (Refl :: xt' :~: xt) ->
          pure $ Res $ App fe xe
        Nothing -> case sUnify xt' (sing @xt) of
          SNothing ->
            lift $ Left $ "Couldn't match type "
              <> pShow (demote @xt) <> " with "
              <> pShow (fromSing xt')
          SJust (refs :: Sing refs) -> withSingI xt' $ withSingI refs $ let
            fe' :: ExprT (DFun (Refine refs xt') (Refine refs yt)) =
              case funMaps @refs @xt' @yt of
                FunMaps -> refineExpr @refs fe
            xe' :: ExprT (Refine refs xt) = refineExpr @refs xe
            in case decideEquality (sRefine refs xt') (sRefine refs (sing @xt)) of
              Nothing -> lift $ Left $
                "Type checker error, unify produced invalid refinements\n"
                <> "function: " <> pShow (demote @ft) <> "\n"
                <> "arg: " <> pShow (demote @xt) <> "\n"
                <> "refs: " <> show (demote @refs) <> "\n"
              Just Refl -> case singInstance (sRefine refs yt) of
                SingInstance -> pure $ Res $ App fe' xe'
    -- TODO This is actually wrong, it should suport unifications from vars to functions
    _ -> lift $ Left "Applied argument to non-function. too many arguments?"

pShow :: DType -> Text
pShow DInt = "Int"
pShow DBool = "Bool"
pShow (DList l) = "[" <> pShow l <> "]"
pShow (DFun a b) = pShow a <> " -> " <> pShow b
pShow (DVar n) = case ['a'..'z'] !!? (fromIntegral n :: Int) of
  Just t -> one t
  Nothing -> "v" <> show n
