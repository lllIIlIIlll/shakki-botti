module Board (parseFen, updateFen, playMove, boardToFen) where

import Types (Board, Move(..))
import Utils (splitMove, squareToCoord)

import Data.List.Split (splitOn, chunksOf)
import Data.List (intercalate)
import Data.Char
import qualified Data.Vector as V

-- example fens
-- 6k1/p3pp1p/1n3bpB/8/1q6/2N4P/PP3PP1/3Q2K1 w KQkq - 0 1
-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

-- parse fen to board
parseFen :: String -> Board
parseFen fenStr = 
  let rows = splitOn "/" (head (words fenStr))
      parsedFenRows = map parseFenRowList rows
  in V.fromList (concat parsedFenRows)

parseFenRowList :: String -> [Int]
parseFenRowList [] = []
parseFenRowList (x:xs) = 
  if isDigit x 
  then replicate (digitToInt x) 0 ++ parseFenRowList xs 
  else case x of
         'P' -> 1  : parseFenRowList xs
         'N' -> 2  : parseFenRowList xs
         'B' -> 3  : parseFenRowList xs
         'R' -> 4  : parseFenRowList xs
         'Q' -> 5  : parseFenRowList xs
         'K' -> 6  : parseFenRowList xs
         'p' -> 7  : parseFenRowList xs
         'n' -> 8  : parseFenRowList xs
         'b' -> 9  : parseFenRowList xs
         'r' -> 10 : parseFenRowList xs
         'q' -> 11 : parseFenRowList xs
         'k' -> 12 : parseFenRowList xs
         _   -> error "not valid fen"

-- updates the fen position string and toggles the turn
updateFen :: String -> String -> String
updateFen currentFen moveStr = 
  let (from', dest') = splitMove moveStr
      promotion = if length moveStr == 5 then Just (last moveStr) else Nothing
      move = Move { from      = squareToCoord from',
                    dest      = squareToCoord dest',
                    promotion = promotion,
                    capture   = Nothing }
   in boardToFen (playMove (parseFen currentFen) move) ++ " " ++ unwords (updateRest (tail (words currentFen)))

updateRest :: [String] -> [String]
updateRest [] = []
updateRest (r:rest)
  | r == "w" = "b" : updateRest rest
  | r == "b" = "w" : updateRest rest
  | otherwise = r  : updateRest rest

boardToFen :: Board -> String
boardToFen board =
  let rows = chunksOf 8 (V.toList board)
      fenRows = map rowToFen rows
  in intercalate "/" fenRows

rowToFen :: [Int] -> String
rowToFen [] = ""
rowToFen (r:row) = 
  case r of
    1  -> "P" ++ rowToFen row
    2  -> "N" ++ rowToFen row
    3  -> "B" ++ rowToFen row
    4  -> "R" ++ rowToFen row
    5  -> "Q" ++ rowToFen row
    6  -> "K" ++ rowToFen row
    7  -> "p" ++ rowToFen row
    8  -> "n" ++ rowToFen row
    9  -> "b" ++ rowToFen row
    10 -> "r" ++ rowToFen row
    11 -> "q" ++ rowToFen row
    12 -> "k" ++ rowToFen row
    0  -> let n = countZeros (r:row)
          in intToDigit n : rowToFen (drop n (r:row))

countZeros :: [Int] -> Int
countZeros [] = 0
countZeros (0:xs) = 1 + countZeros xs
countZeros _ = 0

-- takes a move string and updates the board
playMove :: Board -> Move -> Board
playMove board move = 
  newBoard board (from move) (dest move) (promotion move)

newBoard :: Board -> (Int, Int) -> (Int, Int) -> Maybe Char -> Board
newBoard board (srcRow, srcCol) (destRow, destCol) piece =
  let pieceToMove = case piece of
                      Nothing   -> V.unsafeIndex board (srcRow * 8 + srcCol)
                      Just char -> charToPiece char
  in V.unsafeUpd board [(srcRow * 8 + srcCol, 0), (destRow * 8 + destCol, pieceToMove)]

charToPiece :: Char -> Int
charToPiece char = 
  case char of
    'N' -> 2
    'B' -> 3
    'R' -> 4
    'Q' -> 5
    'n' -> 8
    'b' -> 9
    'r' -> 10
    'q' -> 11