module Day04Spec where

import Test.Hspec
import Day04

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "Example" $ do
      readingsTxt <- readFile "inputs/Day04/example"
      let (drawn, hall) = parseInputFile readingsTxt
      winner drawn hall `shouldBe` Just 4512
    it "Gold star" $ do
      readingsTxt <- readFile "inputs/Day04/input"
      let (drawn, hall) = parseInputFile readingsTxt
      winner drawn hall `shouldBe` Just 33348
  describe "Part 2" $ do
    it "Example" $ do
      readingsTxt <- readFile "inputs/Day04/example"
      let (drawn, hall) = parseInputFile readingsTxt
      winnerSquid drawn hall `shouldBe` Just 1924
    it "Gold star" $ do
      readingsTxt <- readFile "inputs/Day04/input"
      let (drawn, hall) = parseInputFile readingsTxt
      winnerSquid drawn hall `shouldBe` Just 8112
