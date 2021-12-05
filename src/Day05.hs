module Day05 where

import Data.List
import Data.List.Split
import qualified Data.Map as M

data Coordinate = Coordinate Int Int
  deriving (Eq, Ord, Show)

data Line = Line Coordinate Coordinate
  deriving (Show)

mkCoordinate :: String -> Coordinate
mkCoordinate s = Coordinate x y
  where [x, y] = map read $ splitOn "," s

mkLine :: String -> Line
mkLine s = Line (mkCoordinate from) (mkCoordinate to)
  where [from, _, to] = words s

parseInputFile :: String -> [Line]
parseInputFile = map mkLine . lines

rangeBothWays a b
  | a == b = [a]
  | otherwise = enumFromThenTo a (a - signum (a - b)) b

points :: Line -> [Coordinate]
points (Line (Coordinate x1 y1) (Coordinate x2 y2 )) 
  | x1 == x2 = [Coordinate x1 y | y <- [min y1 y2..max y1 y2]]
  | y1 == y2 = [Coordinate x y1 | x <- [min x1 x2..max x1 x2]]
  | otherwise = map (\(x, y) -> Coordinate x y) $ zip (rangeBothWays x1 x2) (rangeBothWays y1 y2)

isDiagonal :: Line -> Bool
isDiagonal (Line (Coordinate x1 y1) (Coordinate x2 y2 )) = and [x1 /= x2, y1 /= y2]

insertLine m l = foldl (\m c -> M.insertWith (+) c 1 m) m $ points l

countOverlaps = length . filter (\(_, n) -> 1 < n) . M.toList . foldl insertLine M.empty

countOverlapsNoDiagonals = countOverlaps . filter (not . isDiagonal)

main :: IO ()
main = do
  input <- readFile "inputs/Day05/example"
  print $ countOverlapsNoDiagonals $ parseInputFile input

