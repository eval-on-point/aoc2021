module Day02Spec where

import Test.Hspec
import Day02
import qualified Day02_02 as S

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "Example" $ do
      rutterTxt <- readFile "inputs/Day02/example"
      let report = readRutter rutterTxt
      rutterProduct report `shouldBe` 150
    it "Gold star" $ do
      rutterTxt <- readFile "inputs/Day02/input"
      let report = readRutter rutterTxt
      rutterProduct report `shouldBe` 2073315
  describe "Part 2" $ do
    it "Example" $ do
      rutterTxt <- readFile "inputs/Day02/example"
      let report = S.readRutter rutterTxt
      S.rutterProduct report `shouldBe` 900
    it "Gold star" $ do
      rutterTxt <- readFile "inputs/Day02/input"
      let report = S.readRutter rutterTxt
      S.rutterProduct report `shouldBe` 1840311528
