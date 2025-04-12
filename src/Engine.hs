module Engine (findBestMove, evaluateBoard) where

import Data.Ord
import Data.List.Split()
import Data.List (sortBy)
import qualified Data.Vector as V

import LegalMoves (findLegalMoves, checkmateDetection) 
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
  )

-- this is the function that the mainloop calls to play engine's move
findBestMove :: Board -> Color -> (Int, Maybe Move)
findBestMove board color = 
  let bestMove = minimax board 6 (-99999999) 99999999 color
  in case bestMove of
      (eval, Just move) -> (eval, Just move)
      (_, Nothing)      -> (99, Nothing)

-- minimax search with alpha-beta-pruning that returns the best evaluation and corresponding move
minimax :: Board -> Int -> Int -> Int -> Color -> (Int, Maybe Move)
minimax board depth alpha beta color =
  let moves = findLegalMoves board color
  in if depth == 0 || null moves 
     then (evaluate board color depth moves, Nothing)
     else if color == White
            then maximize board (setMoveOrder moves) depth alpha beta Nothing
            else minimize board (setMoveOrder moves) depth alpha beta Nothing

-- sorts legal moves to prioritize capture moves to improve alpha-beta-pruning
setMoveOrder :: [Move] -> [Move]
setMoveOrder = sortBy (comparing (Down . capture))

-- takes a list of white's legal moves and returns the move with the highest evaluation
-- updates alpha (white's best evaluation) when a move with better evaluation is found
-- if alpha is greater than beta (black's best evaluation) we stop the search.
maximize :: Board -> [Move] -> Int -> Int -> Int -> Maybe Move -> (Int, Maybe Move)
maximize _ [] _ alpha _ best = (alpha, best)
maximize board (m:moves) depth alpha beta best =
  let (eval, _) = minimax (playMove board m) (depth - 1) alpha beta Black
      newAlpha = max alpha eval
      newBest = if eval > alpha then Just m else best
  in if newAlpha >= beta
     then (newAlpha, newBest)
     else maximize board moves depth newAlpha beta newBest

-- works like maximizer but takes a list of black's legal moves and returns the move with the lowest evaluation
-- updates beta (black's best evaluation) when a move with lower/better evaluation is found
-- if alpha is greater than beta (black's best evaluation) we stop the search.
minimize :: Board -> [Move] -> Int -> Int -> Int -> Maybe Move -> (Int, Maybe Move)
minimize _ [] _ _ beta best = (beta, best)
minimize board (m:moves) depth alpha beta best = 
  let (eval, _) = minimax (playMove board m) (depth - 1) alpha beta White
      newBeta = min beta eval
      newBest = if eval < beta then Just m else best
  in if alpha >= newBeta
     then (newBeta, newBest)
     else minimize board moves depth alpha newBeta newBest

-- evaluates board position, if no legal moves the match has ended, either checkmate or stalemate
evaluate :: Board -> Color -> Int -> [Move] -> Int
evaluate board color depth moves = 
  if null moves
  then if checkmateDetection board color
       then if color == White
            then -100000 - depth
            else 100000  + depth
       else 0 -- stalemate
  else evaluateBoard board

-- calculates the overall evaluation score by combining both material and positional evaluations
evaluateBoard :: Board -> Int
evaluateBoard board = 
  let materialEval = V.foldl' (\acc piece -> acc + getPieceValue piece) 0 board
      positionEval = V.ifoldl' (\acc idx piece -> acc + getPosValue idx piece) 0 board
  in materialEval + positionEval

-- returns the positional evaluation value of a piece based on its location on the board
getPosValue :: Int -> Int -> Int
getPosValue idx 1  = V.unsafeIndex whitePawnPrefCoords idx
getPosValue idx 2  = V.unsafeIndex whiteKnightPrefCoords idx
getPosValue idx 3  = V.unsafeIndex whiteBishopPrefCoords idx
getPosValue idx 4  = V.unsafeIndex whiteRookPrefCoords idx
getPosValue idx 5  = V.unsafeIndex whiteQueenPrefCoords idx
getPosValue idx 7  = V.unsafeIndex blackPawnPrefCoords idx
getPosValue idx 8  = V.unsafeIndex blackKnightPrefCoords idx
getPosValue idx 9  = V.unsafeIndex blackBishopPrefCoords idx
getPosValue idx 10 = V.unsafeIndex blackRookPrefCoords idx
getPosValue idx 11 = V.unsafeIndex blackQueenPrefCoords idx
getPosValue _ _    = 0

-- returns the material evaluation of a piece
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
