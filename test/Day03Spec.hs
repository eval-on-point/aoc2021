module Day03Spec where

import Test.Hspec
import Day03

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "Example" $ do
      readingsTxt <- readFile "inputs/Day03/example"
      let nums = parseReadings readingsTxt
      powerConsumption nums `shouldBe` 198
    it "Gold star" $ do
      readingsTxt <- readFile "inputs/Day03/input"
      let nums = parseReadings readingsTxt
      powerConsumption nums `shouldBe` 3374136
  describe "Part 2" $ do
    it "Example" $ do
      readingsTxt <- readFile "inputs/Day03/example"
      let nums = parseReadings readingsTxt
      lifeSupportRating nums `shouldBe` 230
    it "Gold star" $ do
      readingsTxt <- readFile "inputs/Day03/input"
      let nums = parseReadings readingsTxt
      lifeSupportRating nums `shouldBe` 4432698
