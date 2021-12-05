module Day05Spec where

import Test.Hspec
import Day05

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "Example" $ do
      input <- readFile "inputs/Day05/example"
      let lines = parseInputFile input
      countOverlapsNoDiagonals lines `shouldBe` 5
    it "First gold star" $ do
      input <- readFile "inputs/Day05/input"
      let lines = parseInputFile input
      countOverlapsNoDiagonals lines `shouldBe` 6225
  describe "Part 2" $ do
    it "Example" $ do
      input <- readFile "inputs/Day05/example"
      let lines = parseInputFile input
      countOverlaps lines `shouldBe` 12
    it "Second gold star" $ do
      input <- readFile "inputs/Day05/input"
      let lines = parseInputFile input
      countOverlaps lines `shouldBe` 22116
