module Day01 where

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

increasingSubseqSums :: Ord a => Int -> [a] -> Int
increasingSubseqSums n s  = count (uncurry (<)) $ zip s (drop n s)

countLargerThanPrevious :: Ord a => [a] -> Int
countLargerThanPrevious = increasingSubseqSums 1

countIncreasingTriples :: Ord a => [a] -> Int
countIncreasingTriples = increasingSubseqSums 3

parseReport :: String -> [Int]
parseReport = map read . words

main :: IO ()
main = do
  contents <- readFile "report.txt"
  let parsed = map read $ words contents :: [Integer]
  print (countLargerThanPrevious parsed)
  print (countIncreasingTriples parsed)
