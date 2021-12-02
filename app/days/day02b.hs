module Day02 where

type Depth = Integer
type Forward = Integer
type Aim = Integer

data Coordinate = Coordinate Depth Forward
  deriving (Eq, Show)

data Position = Position Coordinate Aim
  deriving (Eq, Show)

addPositions :: Position -> Position -> Position
addPositions (Position (Coordinate d1 f1) a1)
             (Position (Coordinate d2 f2) a2) =
  Position (Coordinate (d1 + (a1 + a2) * d2) (f1 + f2)) (a1 + a2)

positionProduct :: Position -> Integer
positionProduct (Position (Coordinate d f) _) = d * f

readPosition :: String -> Position
readPosition s = case words s of
  ["forward", n] -> Position (Coordinate (read n) (read n)) 0
  ["down", n] -> Position (Coordinate 0 0) (read n)
  ["up", n] -> Position (Coordinate 0 0) (negate (read n))

type Rutter = [Position]

readRutter :: String -> Rutter
readRutter = map readPosition . lines

followRutter :: Rutter -> Position
followRutter = foldl addPositions (Position (Coordinate 0 0) 0)

ex1 = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

main :: IO ()
main = do
  contents <- readFile "inputs/02_1"
  print (positionProduct $ followRutter (readRutter contents))
