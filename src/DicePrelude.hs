{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module DicePrelude where

import Cast (H0, cast, castPolly)
import Data.Map qualified as M
import TypeCheckerCore (Res)

prelude :: Map Text Res
prelude =
  M.fromList
    [ ("+", cast $ (+) @Int)
    , ("-", cast $ (-) @Int)
    , ("*", cast $ (*) @Int)
    , ("/", cast $ div @Int)
    , ("^", cast $ (^) @Int @Int)
    , ("**", cast $ (^) @Int @Int)
    , ("&&", cast (&&))
    , ("||", cast (||))
    , -- TODO classes so you can compare lists and such?
      (">", cast $ (>) @Int)
    , ("<", cast $ (<) @Int)
    , (">=", cast $ (>=) @Int)
    , ("<=", cast $ (<=) @Int)
    , ("==", cast $ (==) @Int)
    , ("/=", cast $ (/=) @Int)
    , ("++", castPolly @([H0] -> [H0] -> [H0]) (++))
    ,
      ( "ifte"
      , castPolly @(Bool -> H0 -> H0 -> H0)
          (\c t f -> if c then t else f)
      )
    , ("True", cast True)
    , ("False", cast False)
    , ("replicate", castPolly @(Int -> H0 -> [H0]) replicate)
    , -- TODO probably better if this works more like replicateM
      -- may require seperate builtin?
      ("even", cast @(Int -> Bool) even)
    , ("odd", cast @(Int -> Bool) odd)
    , ("id", castPolly @(H0 -> H0) id)
    , ("length", castPolly @([H0] -> Int) length)
    , ("filter", castPolly @((H0 -> Bool) -> [H0] -> [H0]) filter)
    -- TODO more prelude functions
    -- especially (.)
    ]
