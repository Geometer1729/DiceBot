module Main where

import Control.Monad (liftM2)
import Dist (range, times, unsafeSize, Dist)
import Test.Hspec
import Data.Maybe (fromJust)

main :: IO ()
main = do
  when False $
    hspec $ do
      describe "Dist" $ do
        describe "simple" $ do
          it "fmap" $ do
            unsafeSize (range' 1 6 <&> div 2) `shouldBe` (3 :: Int)
          it "<*>" $ do
            unsafeSize ((+) <$> range' 1 6 <*> range' 1 6) `shouldBe` (11 :: Int)
          it "liftM2" $ do
            unsafeSize (liftM2 (+) (range' 1 6) (range' 1 6)) `shouldBe` (11 :: Int)
          it ">>=" $ do
            unsafeSize (range' 1 6 >>= range' 1) `shouldBe` 6
          it "replicateM" $ do
            unsafeSize (sum <$> replicateM 4 (range' 1 4)) `shouldBe` 13
          it "performance" $ do
            unsafeSize (sum $ replicate 5 (range' 1 5)) `shouldBe` 21
          focus $
            it "performance2" $ do
              unsafeSize (10 `times` range' 1 10) `shouldBe` 91

range' :: Int -> Int -> Dist Int
range' = fmap fromJust . range
