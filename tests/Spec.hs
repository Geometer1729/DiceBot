module Main where

import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Dist (Dist, expected, range, times, unsafeSize)
import RollM qualified
import Stats (DistM (runDistM))
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "Dist" $ do
      describe "unit tests" $ do
        it "E(10d10)=55" $
          expected (10 `times` d' 10) `shouldApprox` 55
        it "E(6d6-6d6)=0" $
          expected (6 `times` d' 6 - 6 `times` d' 6) `shouldApprox` 0
        it "E(10d10+10d10)=110" $
          expected (10 `times` d' 10 + 10 `times` d' 10) `shouldApprox` 110

      describe "classes" $ do
        it "fmap" $ do
          unsafeSize (d' 6 <&> div 2) `shouldBe` (3 :: Int)
        it "<*>" $ do
          unsafeSize ((+) <$> d' 6 <*> d' 6) `shouldBe` (11 :: Int)
        it "liftM2" $ do
          unsafeSize (liftM2 (+) (d' 6) (d' 6)) `shouldBe` (11 :: Int)
        it ">>=" $ do
          unsafeSize (d' 6 >>= d') `shouldBe` 6
        it "replicateM" $ do
          unsafeSize (sum <$> replicateM 4 (d' 4)) `shouldBe` 13
        describe "performance" $ do
          it "1" $ do
            unsafeSize (sum $ replicate 5 (d' 5)) `shouldBe` 21
          it "1" $ do
            unsafeSize (5 `times` d' 5) `shouldBe` 21
          it "3" $ do
            unsafeSize (10 `times` d' 10) `shouldBe` 91
          it "4" $ do
            unsafeSize (runMaybeT (runDistM (5 `RollM.times` RollM.range 1 5))) `shouldBe` 21
          it "5" $ do
            unsafeSize (runMaybeT (runDistM (10 `RollM.times` RollM.range 1 10))) `shouldBe` 91

shouldApprox :: Double -> Double -> Expectation
shouldApprox a b = a `shouldSatisfy` (\a' -> abs (a' - b) < 1e-6)

-- strange implementation gives better errors

d' :: Int -> Dist Int
d' = range' 1

range' :: Int -> Int -> Dist Int
range' = fmap fromJust . range
