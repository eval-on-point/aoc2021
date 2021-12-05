module Day01Spec where

import Test.Hspec
import Day01

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "Example" $ do
      reportTxt <- readFile "inputs/Day01/example1"
      let report = parseReport reportTxt
      countLargerThanPrevious report `shouldBe` 7
    it "Gold star" $ do
      reportTxt <- readFile "inputs/Day01/input"
      let report = parseReport reportTxt
      countLargerThanPrevious report `shouldBe` 1791
  describe "Part 2" $ do
    it "Example" $ do
      reportTxt <- readFile "inputs/Day01/example2"
      let report = parseReport reportTxt
      countIncreasingTriples report `shouldBe` 5
    it "Gold star" $ do
      reportTxt <- readFile "inputs/Day01/input"
      let report = parseReport reportTxt
      countIncreasingTriples report `shouldBe` 1822
