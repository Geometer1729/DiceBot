import Criterion.Main

import Parser(parseRoll)
import Stats (getExpected)
import Dist(Dist,range,expected,times)
import Data.Maybe (fromJust)

benchExp :: Text -> Benchmark
benchExp w = bench (toString w) $ nf (fmap getExpected) (parseRoll w)

main :: IO ()
main = defaultMain
  [ benchExp "10d10"
  , benchExp "20d20"
  , benchExp "30d30"
  , benchExp "40d40"
  , benchExp "50d50"
  , bench "10d10-direct" $ nf f 10
  , bench "50d50-direct" $ nf f 50
  , bench "10d10-justDist" $ nf (10 `times`) (range' 1 10)
  , bench "50d50-justDist" $ nf (50 `times`) (range' 1 50)
  , bench "10d10-justDist-last-pass" $ nf (2 `times`) (5 `times` range' 1 10)
  , bench "50d50-justDist-last-pass" $ nf (2 `times`) (25 `times` range' 1 50)
  ]

f :: Int -> Double
f n = expected $ n `times` range' 1 n

range' :: Int -> Int -> Dist Int
range' = fmap fromJust . range
