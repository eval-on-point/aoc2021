module Day04 where

import Data.Maybe
import Data.List
import qualified Data.Text as T

type DrawnNumbers = [Int]
type BingoBoard = [[Int]]
type BingoHall = [Maybe BingoBoard]

winning :: BingoBoard -> DrawnNumbers -> Bool
winning board drawn = not . null . filter (all $ flip elem drawn) $ board ++ transpose board

score :: DrawnNumbers -> BingoBoard -> Maybe Int
score drawn board
  | winning board drawn = Just (sum [s | s <- concat board, notElem s drawn] * head drawn) 
  | otherwise = Nothing

roundWinners :: DrawnNumbers -> BingoHall -> [Maybe BingoBoard]
roundWinners drawn board = filter (isJust . maybe Nothing (score drawn)) board

roundWinner :: DrawnNumbers -> BingoHall -> Maybe BingoBoard
roundWinner drawn board = minimumBy (\b1 b2 -> compare
                                (maybe Nothing (score drawn) b2)
                                (maybe Nothing (score drawn) b1)) (Nothing:board)

roundWinnerScore :: DrawnNumbers -> BingoHall -> Maybe Int
roundWinnerScore drawn board = maximum . map (maybe Nothing (score drawn)) $ board

-- winners :: DrawnNumbers -> BingoHall -> [Maybe BingoBoard]
winner drawn hall = head $ filter isJust $ snd $ mapAccumL (\a b -> (b:a, roundWinnerScore a hall)) [] drawn

winnerSquid drawn hall = last $ filter isJust $ snd $ mapAccumL (\(a, c) b -> let roundwinner = roundWinner (b:a) (hall \\ c)
                                                 in ((b:a, roundWinners (b:a) (hall \\ c) ++ c), (maybe Nothing (score (b:a))) roundwinner))
                         ([],[]) drawn

toDrawn :: T.Text -> [Int]
toDrawn s = map (read . T.unpack) $ T.splitOn (T.pack ",") s

toBoard :: T.Text -> BingoBoard
toBoard s = map (map read . words) $ lines $ T.unpack s

parseInputFile :: String -> (DrawnNumbers, BingoHall)
parseInputFile s = (toDrawn $ head paragraphs, map (Just . toBoard) $ tail paragraphs)
  where paragraphs = T.splitOn (T.pack "\n\n") $ T.pack s

main :: IO ()
main = do
  contents <- readFile "inputs/04_01"
  let l = T.splitOn (T.pack "\n\n") $ T.pack contents
      d = toDrawn $ head l
      b = map (Just . toBoard) $ tail l
  print $ winnerSquid d b
