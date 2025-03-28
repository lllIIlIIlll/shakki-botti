module Engine (findBestMove) where

import Data.List(maximumBy, minimumBy)
import Data.Ord
import Data.List.Split()
import qualified Data.Vector as V

import Utils (toggleColor)
import LegalMoves (findLegalMoves, checkmateDetection, mapPieces) 
import Board (playMove)
import Types (Color(..), Piece(..), Board)
import PieceSquareTables (
  whitePawnPrefCoords,
  blackPawnPrefCoords,
  whiteKnightPrefCoords,
  blackKnightPrefCoords,
  whiteBishopPrefCoords,
  blackBishopPrefCoords,
  whiteRookPrefCoords,
  blackRookPrefCoords,
  whiteQueenPrefCoords,
  blackQueenPrefCoords,
  whiteKingPrefCoords,
  blackKingPrefCoords
  )

-- this is the function that the mainloop calls to play engine's move
findBestMove :: Board -> String -> (Int, String)
findBestMove board fen = 
  let color = if (words fen) !! 1 == "w" then White else Black
      bestMove = minimax board 4 color
  in case bestMove of
      (eval, Just move) -> (eval, move)
      (_, Nothing)      -> (99, "")

minimax :: Board -> Int -> Color -> (Int, Maybe String)
minimax board depth color
  | depth == 0 = 
    let moves = findLegalMoves board color
    in (evaluate board color depth moves, Nothing)
  | otherwise = 
    let moves = findLegalMoves board color
    in if null moves
       then (evaluate board color depth moves, Nothing)
       else
         let evals = [(minimax (playMove board move) (depth - 1) (toggleColor color), move) | move <- moves]
             bestTuple = if color == White
                         then maximumBy (comparing (fst . fst)) evals
                         else minimumBy (comparing (fst . fst)) evals
         in (fst (fst bestTuple), Just (snd bestTuple))
      
evaluate :: Board -> Color -> Int -> [String] -> Int
evaluate board color depth moves = 
  if null moves
  then if checkmateDetection board color
       then if color == White
            then -100000 - depth
            else 100000  + depth
       else 0 -- stalemate
  else evaluateBoard board
      
evaluateBoard :: Board -> Int
evaluateBoard board = 
  let materialEval = V.sum (V.map getPieceValue board)
      positionEval = calculatePosEval (mapPieces board) 
  in materialEval + positionEval

calculatePosEval :: [((Int, Int), Int)] -> Int
calculatePosEval [] = 0
calculatePosEval (((row, col), piece):pieces) = 
  case piece of 
    1  -> whitePawnPrefCoords   V.! ((row * 8) + col) + calculatePosEval pieces
    2  -> whiteKnightPrefCoords V.! ((row * 8) + col) + calculatePosEval pieces
    3  -> whiteBishopPrefCoords V.! ((row * 8) + col) + calculatePosEval pieces
    4  -> whiteRookPrefCoords   V.! ((row * 8) + col) + calculatePosEval pieces
    5  -> whiteQueenPrefCoords  V.! ((row * 8) + col) + calculatePosEval pieces
    6  -> calculatePosEval pieces -- whiteKingPrefCoords   V.! ((row * 8) + col) 
    7  -> blackPawnPrefCoords   V.! ((row * 8) + col) + calculatePosEval pieces
    8  -> blackKnightPrefCoords V.! ((row * 8) + col) + calculatePosEval pieces
    9  -> blackBishopPrefCoords V.! ((row * 8) + col) + calculatePosEval pieces
    10 -> blackRookPrefCoords   V.! ((row * 8) + col) + calculatePosEval pieces
    11 -> blackQueenPrefCoords  V.! ((row * 8) + col) + calculatePosEval pieces
    12 -> calculatePosEval pieces -- blackKingPrefCoords   V.! ((row * 8) + col)

getPieceValue :: Int -> Int
getPieceValue 0  = 0     -- empty square
getPieceValue 1  = 100   -- white pawn
getPieceValue 2  = 300   -- white knight
getPieceValue 3  = 300   -- white bishop
getPieceValue 4  = 500   -- white rook
getPieceValue 5  = 900   -- white queen
getPieceValue 6  = 0     -- white king
getPieceValue 7  = -100  -- black pawn
getPieceValue 8  = -300  -- black knight
getPieceValue 9  = -300  -- black bishop
getPieceValue 10 = -500  -- black rook
getPieceValue 11 = -900  -- black queen
getPieceValue 12 = 0     -- black king
