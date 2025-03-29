module Engine (findBestMove, evaluateBoard) where

import Data.List(maximumBy, minimumBy)
import Data.Ord
import Data.List.Split()
import qualified Data.Vector as V

import Utils (toggleColor, coordToSquare)
import LegalMoves (findLegalMoves, checkmateDetection, mapPieces) 
import Board (playMove)
import Types (Color(..), Move(..), Board)
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
  --whiteKingPrefCoords,
  --blackKingPrefCoords
  )

-- this is the function that the mainloop calls to play engine's move
findBestMove :: Board -> String -> (Int, String)
findBestMove board fen = 
  let color = if words fen !! 1 == "w" then White else Black
      bestMove = minimax board 4 color
  in case bestMove of
      (eval, Just move) ->
        let moveStr = coordToSquare (from move) ++ coordToSquare (dest move)  
        in case promotion move of
             Just p -> (eval, moveStr ++ [p])
             Nothing        -> (eval, moveStr)
      (_, Nothing)      -> (99, "")

minimax :: Board -> Int -> Color -> (Int, Maybe Move)
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
      
evaluate :: Board -> Color -> Int -> [Move] -> Int
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
    1  -> V.unsafeIndex whitePawnPrefCoords   ((row * 8) + col) + calculatePosEval pieces
    2  -> V.unsafeIndex whiteKnightPrefCoords ((row * 8) + col) + calculatePosEval pieces
    3  -> V.unsafeIndex whiteBishopPrefCoords ((row * 8) + col) + calculatePosEval pieces
    4  -> V.unsafeIndex whiteRookPrefCoords   ((row * 8) + col) + calculatePosEval pieces
    5  -> V.unsafeIndex whiteQueenPrefCoords  ((row * 8) + col) + calculatePosEval pieces
    6  -> calculatePosEval pieces -- V.unsafeIndex whiteKingPrefCoords ((row * 8) + col) 
    7  -> V.unsafeIndex blackPawnPrefCoords   ((row * 8) + col) + calculatePosEval pieces
    8  -> V.unsafeIndex blackKnightPrefCoords ((row * 8) + col) + calculatePosEval pieces
    9  -> V.unsafeIndex blackBishopPrefCoords ((row * 8) + col) + calculatePosEval pieces
    10 -> V.unsafeIndex blackRookPrefCoords   ((row * 8) + col) + calculatePosEval pieces
    11 -> V.unsafeIndex blackQueenPrefCoords  ((row * 8) + col) + calculatePosEval pieces
    12 -> calculatePosEval pieces -- blackKingPrefCoords   V.! ((row * 8) + col)
    _  -> error "not a valid piece"

getPieceValue :: Int -> Int
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
getPieceValue _  = 0
