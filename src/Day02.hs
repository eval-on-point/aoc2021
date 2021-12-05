module Day02 where

type Depth = Integer
type Forward = Integer

data Coordinate = Coordinate Depth Forward
  deriving (Eq, Ord, Show)

addCoords :: Coordinate -> Coordinate -> Coordinate
addCoords (Coordinate a b) (Coordinate c d) = Coordinate (a + c) (b + d)

coordProduct :: Coordinate -> Integer
coordProduct (Coordinate d f) = d * f

readDirection :: String -> Coordinate
readDirection s = case words s of
  ["forward", n] -> Coordinate 0 (read n)
  ["down", n] -> Coordinate (read n) 0
  ["up", n] -> Coordinate (negate (read n)) 0

type Rutter = [Coordinate]

readRutter :: String -> Rutter
readRutter = map readDirection . lines

followRutter :: Rutter -> Coordinate
followRutter = foldl addCoords (Coordinate 0 0)

rutterProduct :: Rutter -> Integer
rutterProduct = coordProduct . followRutter
