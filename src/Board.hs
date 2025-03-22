module Board (parseFen, updateFen, playMove) where

import Types (Color(..), Piece(..), Board)
import Utils (squareToCoord, splitMove)

import Data.List.Split (splitOn)
import Data.Char

-- example fens
-- 6k1/p3pp1p/1n3bpB/8/1q6/2N4P/PP3PP1/3Q2K1 w KQkq - 0 1
-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

-- parse fen to board
parseFen :: String -> Board
parseFen fenStr = 
  let rows = splitOn "/" (head (words fenStr))
  in map parseFenRow rows

parseFenRow :: String -> [Maybe (Color, Piece)]
parseFenRow [] = []
parseFenRow (x:xs) = 
  if isDigit x 
  then replicate (digitToInt x) Nothing ++ parseFenRow xs 
  else case x of 
         'r' -> Just (Black, Rook)   : parseFenRow xs
         'n' -> Just (Black, Knight) : parseFenRow xs
         'b' -> Just (Black, Bishop) : parseFenRow xs
         'q' -> Just (Black, Queen)  : parseFenRow xs
         'k' -> Just (Black, King)   : parseFenRow xs
         'p' -> Just (Black, Pawn)   : parseFenRow xs
         'R' -> Just (White, Rook)   : parseFenRow xs
         'N' -> Just (White, Knight) : parseFenRow xs
         'B' -> Just (White, Bishop) : parseFenRow xs
         'Q' -> Just (White, Queen)  : parseFenRow xs
         'K' -> Just (White, King)   : parseFenRow xs
         'P' -> Just (White, Pawn)   : parseFenRow xs
         _   -> error "not valid fen"

-- updates the fen position string and toggles the turn
updateFen :: String -> String -> String
updateFen currentFen move = 
  (boardToFen (playMove (parseFen currentFen) move)) 
  ++ " " ++ unwords (updateRest (tail (words currentFen)))

updateRest :: [String] -> [String]
updateRest [] = []
updateRest (r:rest)
  | r == "w" = "b" : updateRest rest
  | r == "b" = "w" : updateRest rest
  | otherwise = r  : updateRest rest

boardToFen :: Board -> String
boardToFen [] = ""
boardToFen [b] = rowToFen b
boardToFen (b:board) = rowToFen b ++ "/" ++ boardToFen board

rowToFen :: [Maybe (Color, Piece)] -> String
rowToFen [] = ""
rowToFen (r:row) = 
  case r of 
    Just (Black, Rook)   -> "r" ++ rowToFen row
    Just (Black, Knight) -> "n" ++ rowToFen row
    Just (Black, Bishop) -> "b" ++ rowToFen row
    Just (Black, Queen)  -> "q" ++ rowToFen row
    Just (Black, King)   -> "k" ++ rowToFen row
    Just (Black, Pawn)   -> "p" ++ rowToFen row
    Just (White, Rook)   -> "R" ++ rowToFen row
    Just (White, Knight) -> "N" ++ rowToFen row
    Just (White, Bishop) -> "B" ++ rowToFen row
    Just (White, Queen)  -> "Q" ++ rowToFen row
    Just (White, King)   -> "K" ++ rowToFen row
    Just (White, Pawn)   -> "P" ++ rowToFen row
    Nothing              -> let n = countNothings (r:row)
                            in [intToDigit n] ++ rowToFen (drop n (r:row))

countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing:xs) = 1 + countNothings xs
countNothings _ = 0

-- no castling, en passant or promotion yet.
-- these are also used when checking if the king is checked.
playMove :: Board -> String -> Board
playMove board move = 
  let (srcSq, destSq) = splitMove move
  in newBoard board (squareToCoord srcSq) (squareToCoord destSq)

newBoard :: Board -> (Int, Int) -> (Int, Int) -> Board
newBoard board src dest =
  let pieceToMove = (board !! (fst src)) !! (snd src)
      boardWithoutPiece = updateBoard board src Nothing
  in updateBoard boardWithoutPiece dest pieceToMove

updateBoard :: Board -> (Int, Int) -> Maybe (Color, Piece) -> Board
updateBoard board coord update = 
  let row = board !! (fst coord)
      updatedRow = take (snd coord) row ++ [update] ++ drop ((snd coord) + 1) row
  in take (fst coord) board ++ [updatedRow] ++ drop ((fst coord) + 1) board