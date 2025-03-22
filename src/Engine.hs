module Engine (findLegalMoves) where

import Data.List()
import Data.List.Split()

import Utils (coordToSquare, splitMove, checkBounds)
import Board (playMove)
import Types (Color(..), Piece(..), Board)

-- first generates pseudo-legal moves and then filters out ones that leaves the king in check
findLegalMoves :: Board -> String -> [String]
findLegalMoves board fen = 
  let color = if (words fen) !! 1 == "w" then White else Black
      piecesWithCoords = mapPieces board
      pseudoLegal = listLegalMoves piecesWithCoords color board
      legalMoves = filter (\move -> not (kingInCheck board move color)) pseudoLegal
  in legalMoves

-- creates a list that contains a tuple of a square's coordinate and a piece if there is one
mapPieces :: Board -> [((Int, Int), Maybe (Color, Piece))]
mapPieces board = 
  [((row, col), maybePiece) | (row, rowList) <- zip [0..] board, (col, maybePiece) <- zip [0..] rowList]

listLegalMoves :: [((Int, Int), Maybe (Color, Piece))] -> Color -> Board -> [String]
listLegalMoves [] _ _ = []
listLegalMoves (((row, col), maybePiece):rest) color board =
  case maybePiece of
    Just (color', piece') ->
      if color' == color
      then case piece' of
             Rook   -> findLegalRookMoves board (row, col) color   ++ listLegalMoves rest color board
             Knight -> findLegalKnightMoves board (row, col) color ++ listLegalMoves rest color board
             Bishop -> findLegalBishopMoves board (row, col) color ++ listLegalMoves rest color board
             Queen  -> findLegalQueenMoves board (row, col) color  ++ listLegalMoves rest color board
             King   -> findLegalKingMoves board (row, col) color   ++ listLegalMoves rest color board
             Pawn   -> findLegalPawnMoves board (row, col) color   ++ listLegalMoves rest color board
      else listLegalMoves rest color board
    Nothing -> listLegalMoves rest color board

-- sliding pieces
findLegalRookMoves :: Board -> (Int, Int) -> Color -> [String]
findLegalRookMoves board position color = 
  movesInDirection board position position (0, 1) color
  ++ movesInDirection board position position (0, -1) color
  ++ movesInDirection board position position (1, 0) color
  ++ movesInDirection board position position (-1, 0) color

findLegalBishopMoves :: Board -> (Int, Int)-> Color -> [String]
findLegalBishopMoves board position color = 
  movesInDirection board position position (1, 1) color
  ++ movesInDirection board position position (1, -1) color
  ++ movesInDirection board position position (-1, 1) color
  ++ movesInDirection board position position (-1, -1) color

findLegalQueenMoves :: Board -> (Int, Int) -> Color -> [String]
findLegalQueenMoves board position color = 
  movesInDirection board position position (0, 1) color
  ++ movesInDirection board position position (0, -1) color
  ++ movesInDirection board position position (1, 0) color
  ++ movesInDirection board position position (-1, 0) color
  ++ movesInDirection board position position (1, 1) color
  ++ movesInDirection board position position (1, -1) color
  ++ movesInDirection board position position (-1, 1) color
  ++ movesInDirection board position position (-1, -1) color

-- generates legal moves for sliding pieces
movesInDirection :: Board -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Color -> [String]
movesInDirection board startSq (row, col) (x, y) color = 
  if checkBounds (row + x, col + y)
  then let nextSq = board !! (row + x) !! (col + y)
           moveStr = coordToSquare startSq ++ coordToSquare (row + x, col + y)
       in case nextSq of
            Nothing -> 
              moveStr : movesInDirection board startSq ((row + x), (col + y)) (x, y) color
            Just (pieceColor, _ ) -> 
              if pieceColor == color
              then []
              else [moveStr]
  else []

-- non-sliding pieces
findLegalKnightMoves :: Board -> (Int, Int) -> Color -> [String]
findLegalKnightMoves board (row, col) color = 
  let moves = [(2, 1), (2, -1), (-2, 1), (-2, -1), (1, 2), (1, -2), (-1, 2), (-1, -2)]
      knigthMoves = [(row + x, col + y) | (x, y) <- moves, checkBounds (row + x, col + y)]
  in checkSquare board (row, col) knigthMoves color

findLegalKingMoves :: Board -> (Int, Int) -> Color -> [String]
findLegalKingMoves board (row, col) color =
  let moves = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
      kingMoves = [(row + x, col + y) | (x, y) <- moves, checkBounds (row + x, col + y)]
  in checkSquare board (row, col) kingMoves color

-- generates legal moves for non-sliding pieces
checkSquare :: Board -> (Int, Int) -> [(Int, Int)] -> Color -> [String]
checkSquare _ _ [] _ = []
checkSquare board startSq ((row, col):rest) color = 
  let moveStr = coordToSquare startSq ++ coordToSquare (row, col)
  in case board !! row !! col of
       Nothing ->
         moveStr : checkSquare board startSq rest color
       Just (pieceColor, _) ->
         if pieceColor == color
         then checkSquare board startSq rest color
         else moveStr : checkSquare board startSq rest color

findLegalPawnMoves :: Board -> (Int, Int) -> Color -> [String]
findLegalPawnMoves board (row, col) color =
  let moves = isFirstMove row color
      pawnMoves = [(row + x , col + y) | (x, y) <- moves, checkBounds (row + x, col + y)]
      pawnCaptures = [(row + x, col + y) | (x, y) <- if color == White then [(-1, 1), (-1, -1)] else [(1, -1), (1, 1)], checkBounds (row + x , col + y)]
  in checkPawnMoves board (row, col) pawnMoves color ++ checkPawnCaptures board (row, col) pawnCaptures color

-- helper for pawnMoves
isFirstMove :: Int -> Color -> [(Int, Int)]
isFirstMove row color
  | color == White && row == 6 = [(-1, 0), (-2, 0)]
  | color == Black && row == 1 = [(1, 0), (2, 0)]
  | color == White = [(-1, 0)]
  | otherwise = [(1, 0)]

checkPawnMoves :: Board -> (Int, Int) -> [(Int, Int)] -> Color -> [String]
checkPawnMoves _ _ [] _ = []
checkPawnMoves board startSq ((row, col):rest) color =
  let moveStr = coordToSquare startSq ++ coordToSquare (row, col)
      twoSqMove = abs ((fst startSq) - row) > 1
  in if twoSqMove
     then case board !! (rowAdjustment color row) !! (col) of
            Nothing -> 
              case board !! row !! col of
                Nothing ->
                  moveStr : checkPawnMoves board startSq rest color
                _ -> 
                  checkPawnMoves board startSq rest color
            _ -> []
     else case board !! row !! col of
              Nothing ->
                moveStr : checkPawnMoves board startSq rest color
              _ ->
                checkPawnMoves board startSq rest color

-- checks if pawn capture offsets target an enemy piece, returns list of possible captures
checkPawnCaptures :: Board -> (Int, Int) -> [(Int, Int)] -> Color -> [String]
checkPawnCaptures _ _ [] _ = [] 
checkPawnCaptures board startSq ((row, col):rest) color = 
  let moveStr = coordToSquare startSq ++ coordToSquare (row, col)
  in case board !! row !! col of
    Nothing ->
      checkPawnCaptures board startSq rest color
    Just (pieceColor, _) ->
      if pieceColor == color
      then checkPawnCaptures board startSq rest color
      else moveStr : checkPawnCaptures board startSq rest color

-- different offset for white and black when pawn tries to move over piece
rowAdjustment :: Color -> Int -> Int
rowAdjustment (White) x = x + 1
rowAdjustment _  x = x - 1

-- simulates board after pseudo-legal move and checks if truly legal
-- (king is not in check after the move)
kingInCheck :: Board -> String -> Color -> Bool
kingInCheck board move color = 
  let boardAfterMove = playMove board move
      opponentColor = if color == White then Black else White
      kingCoord = kingLocation (mapPieces boardAfterMove) color
      enemyMoves = listLegalMoves (mapPieces boardAfterMove) opponentColor boardAfterMove
  in elem kingCoord (map (snd . splitMove) enemyMoves)

kingLocation :: [((Int, Int), Maybe (Color, Piece))] -> Color -> String
kingLocation [] _ = ""
kingLocation (((row, col), maybePiece):rest) color = 
  case maybePiece of
    Just (color', King) ->
      if color' == color
        then coordToSquare (row, col)
        else kingLocation rest color
    _ -> kingLocation rest color
