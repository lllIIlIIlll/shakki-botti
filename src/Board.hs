module Board (fenToGameState, playMove, updateGameState) where

import Types (Board, Move(..), GameState(..), Color(..))
import Utils (toggleColor)

import Data.List.Split (splitOn)
import Data.Char
import qualified Data.Vector as V

-- example fens
-- 6k1/p3pp1p/1n3bpB/8/1q6/2N4P/PP3PP1/3Q2K1 w KQkq - 0 1
-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

-- fen parser
fenToGameState :: String -> GameState
fenToGameState fenStr = 
  let fenParts  = words fenStr
      boardRows = splitOn "/" (head fenParts)
  in GameState {
        board        = V.fromList (concatMap parseFenRowList boardRows),
        turn         = if fenParts !! 1 == "w" then White else Black,
        wKingCastle  = elem 'K' (fenParts !! 2),
        wQueenCastle = elem 'Q' (fenParts !! 2),
        bKingCastle  = elem 'k' (fenParts !! 2),
        bQueenCastle = elem 'q' (fenParts !! 2),
        enPassant    = Nothing,
        halfMove     = 0,
        fullMove     = 0
      }

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

-- new update fen
updateGameState :: GameState -> Move -> GameState
updateGameState gameState move = 
  let newBoard = playMove (board gameState) move
      newTurn  = toggleColor (turn gameState)
      -- castling not implemented yet
      newWKingCastle  = True
      newWQueenCastle = True
      newBKingCastle  = True
      newBQueenCastle = True
      -- gamestate is not updating these yet
      newEnPassant = Nothing
      newHalfMove = 0
      newFullMove = 0
   in GameState {
        board        = newBoard,
        turn         = newTurn,
        wKingCastle  = newWKingCastle,
        wQueenCastle = newWQueenCastle,
        bKingCastle  = newBKingCastle,
        bQueenCastle = newBQueenCastle,
        enPassant    = newEnPassant,
        halfMove     = newHalfMove,
        fullMove     = newFullMove
   }

-- takes a move and updates the board
playMove :: Board -> Move -> Board
playMove board move = 
  newBoard board (from move) (dest move) (promotion move) (castle move)

newBoard :: Board -> (Int, Int) -> (Int, Int) -> Maybe Char -> Maybe ((Int, Int), (Int, Int)) -> Board
newBoard board (srcRow, srcCol) (destRow, destCol) promoPiece castle =
  let pieceToMove = case promoPiece of
                      Nothing   -> V.unsafeIndex board (srcRow * 8 + srcCol)
                      Just char -> charToPiece char
  in case castle of
      Just ((sRow, sCol), (dRow, dCol)) -> 
        let rook = V.unsafeIndex board (sRow * 8 + sCol)
        in V.unsafeUpd board [(srcRow * 8 + srcCol, 0), (destRow * 8 + destCol, pieceToMove), (sRow * 8 + sCol, 0), (dRow * 8 + dCol, rook)]
      Nothing -> 
        V.unsafeUpd board [(srcRow * 8 + srcCol, 0), (destRow * 8 + destCol, pieceToMove)]

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
    _   -> error "not a valid piece code"