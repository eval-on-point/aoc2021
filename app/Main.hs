module Main where

import Test.Hspec

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

increasingSubseqSums :: Ord a => Int -> [a] -> Int
increasingSubseqSums n s  = count (uncurry (<)) $ zip s (drop n s)

countLargerThanPrevious :: Ord a => [a] -> Int
countLargerThanPrevious = increasingSubseqSums 1

countIncreasingTriples :: Ord a => [a] -> Int
countIncreasingTriples = increasingSubseqSums 3

test :: IO ()
test = hspec $ do
  describe "AoC day 1" $ do
    it "First example is correct" $ do
      countLargerThanPrevious exampleReport `shouldBe` (7 :: Int)
    it "Second example is correct" $ do
      countIncreasingTriples exampleReport2 `shouldBe` (5 :: Int)
    where exampleReport = [199, 200, 208, 210, 200,
                           207, 240, 269, 260, 263] :: [Int]
          exampleReport2 = [607, 618, 618, 617, 647, 716, 769, 792] :: [Int]


main :: IO ()
main = do
  contents <- readFile "report.txt"
  let parsed = map read $ words contents :: [Integer]
  print (countLargerThanPrevious parsed)
  print (countIncreasingTriples parsed)
