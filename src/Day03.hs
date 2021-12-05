module Day03 where

import Data.List

type BinNum = [Bool]

mostFrequent :: BinNum -> Bool
mostFrequent s = (length s - totalTrue) <= totalTrue
  where totalTrue =  sum . map fromEnum $ s

readBin :: BinNum -> Int
readBin = snd . foldr (\b (acc, r) ->
                         (acc + 1, r + (fromEnum b * 2 ^ acc)))
                      (0, 0)

complement :: BinNum -> BinNum
complement = map not

gammaRate :: [BinNum] -> Int
gammaRate = readBin . map mostFrequent . transpose

epsilonRate :: [BinNum] -> Int
epsilonRate = readBin . complement . map mostFrequent . transpose

powerConsumption :: [BinNum] -> Int
powerConsumption s = epsilonRate s * gammaRate s

toBinNum :: String -> BinNum
toBinNum = map (toEnum . read . (:""))

diagnosticFilterIdx :: (BinNum -> BinNum) -> Int -> [BinNum] -> [BinNum]
diagnosticFilterIdx _ 0 bs = bs
diagnosticFilterIdx criterionMatch idx bs =
  if length matches == 1
  then matches
  else diagnosticFilterIdx criterionMatch (idx - 1) matches
  where matches = map fst . filter snd . zip bs $ criterionMatch (transpose bs !! (length (head bs) - idx))

diagnosticFilter :: (BinNum -> BinNum) -> [BinNum] -> Int
diagnosticFilter criterionMatch bs = readBin . head $ diagnosticFilterIdx criterionMatch (length (head bs)) bs

oxygenGeneratorCriterion :: BinNum -> BinNum
oxygenGeneratorCriterion b = map (==mostcommon) b
  where mostcommon = mostFrequent b

oxygenGeneratorRating :: [BinNum] -> Int
oxygenGeneratorRating = diagnosticFilter oxygenGeneratorCriterion

co2ScrubberCriterion :: BinNum -> BinNum
co2ScrubberCriterion  = map not . oxygenGeneratorCriterion

co2ScrubberRating :: [BinNum] -> Int
co2ScrubberRating = diagnosticFilter co2ScrubberCriterion

lifeSupportRating :: [BinNum] -> Int
lifeSupportRating bs = oxygenGeneratorRating bs * co2ScrubberRating bs

parseReadings :: String -> [BinNum]
parseReadings = map toBinNum . lines
