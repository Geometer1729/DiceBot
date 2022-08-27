module Main where

import Test.Hspec (describe, hspec, it, shouldContain)

-- TODO parser tests
main :: IO ()
main = hspec $ do
  describe "Lib.hello" $ do
    it "contains the world emoji" $ do
      "ab" `shouldContain` "a"
