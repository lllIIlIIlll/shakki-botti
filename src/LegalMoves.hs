module LegalMoves (findLegalMoves, checkmateDetection, mapPieces) where

import qualified Data.Vector as V

import Utils (coordToSquare, splitMove, checkBounds, toggleColor)
import Board (playMove)
import Types (Color(..), Board)

-- first generate pseudo-legal moves and then filter out ones that leaves the king in check
findLegalMoves :: Board -> Color -> [String]
findLegalMoves board color = 
  let piecesWithCoords = mapPieces board
      pseudoLegal = listLegalMoves piecesWithCoords color board
      legalMoves = filter (\move -> not (kingInCheck board move color)) pseudoLegal
  in legalMoves

-- creates a list that contains a tuple of a piece's coordinate and a piece
mapPieces :: Board -> [((Int, Int), Int)]
mapPieces board = 
  [((row, col), piece) 
  | row <- [0..7]
  , col <- [0..7]
  , let idx = row * 8 + col
  , let piece = V.unsafeIndex board idx
  , piece /= 0
  ]

listLegalMoves :: [((Int, Int), Int)] -> Color -> Board -> [String]
listLegalMoves [] _ _ = []
listLegalMoves (((row, col), piece):rest) color board =
  if color == White
  then case piece of
    1 -> findLegalPawnMoves board (row, col) color   ++ listLegalMoves rest color board
    2 -> findLegalKnightMoves board (row, col) color ++ listLegalMoves rest color board
    3 -> findLegalBishopMoves board (row, col) color ++ listLegalMoves rest color board
    4 -> findLegalRookMoves board (row, col) color   ++ listLegalMoves rest color board
    5 -> findLegalQueenMoves board (row, col) color  ++ listLegalMoves rest color board
    6 -> findLegalKingMoves board (row, col) color   ++ listLegalMoves rest color board
    _ -> listLegalMoves rest color board
  else case piece of
    7  -> findLegalPawnMoves board (row, col) color   ++ listLegalMoves rest color board
    8  -> findLegalKnightMoves board (row, col) color ++ listLegalMoves rest color board
    9  -> findLegalBishopMoves board (row, col) color ++ listLegalMoves rest color board
    10 -> findLegalRookMoves board (row, col) color   ++ listLegalMoves rest color board
    11 -> findLegalQueenMoves board (row, col) color  ++ listLegalMoves rest color board
    12 -> findLegalKingMoves board (row, col) color   ++ listLegalMoves rest color board
    _ -> listLegalMoves rest color board

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
  then let nextSq = V.unsafeIndex board ((row + x) * 8 + (col + y))
           moveStr = coordToSquare startSq ++ coordToSquare (row + x, col + y)
       in case nextSq of
            0 -> 
              moveStr : movesInDirection board startSq ((row + x), (col + y)) (x, y) color
            code
              | color == White && code <= 6 -> []
              | color == Black && code >= 7 -> []
              | otherwise                   -> [moveStr]
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
  in case V.unsafeIndex board (row * 8 + col) of
       0 ->
         moveStr : checkSquare board startSq rest color
       code
         | color == White && code <= 6 -> checkSquare board startSq rest color
         | color == Black && code >= 7 -> checkSquare board startSq rest color
         | otherwise                   -> moveStr : checkSquare board startSq rest color

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
     then case V.unsafeIndex board ((rowAdjustment color row) * 8 + col) of
            0 -> 
              case board V.! (row * 8 + col) of
                0 ->
                  moveStr : checkPawnMoves board startSq rest color
                _ -> 
                  checkPawnMoves board startSq rest color
            _ -> []
     else case board V.! (row * 8 + col) of
              0 ->
                -- check if promotion available
                if color == White && row == 0
                then [moveStr ++ "N", moveStr ++ "B", moveStr ++ "R", moveStr ++ "Q"] ++ checkPawnMoves board startSq rest color
                else if color == Black && row == 7
                     then [moveStr ++ "n", moveStr ++ "b", moveStr ++ "r", moveStr ++ "q"] ++ checkPawnMoves board startSq rest color
                     else moveStr : checkPawnMoves board startSq rest color
              _ ->
                checkPawnMoves board startSq rest color

-- checks if pawn capture offset target an enemy piece, returns list of possible captures
checkPawnCaptures :: Board -> (Int, Int) -> [(Int, Int)] -> Color -> [String]
checkPawnCaptures _ _ [] _ = [] 
checkPawnCaptures board startSq ((row, col):rest) color = 
  let moveStr = coordToSquare startSq ++ coordToSquare (row, col)
  in case V.unsafeIndex board (row * 8 + col) of
    0 ->
      checkPawnCaptures board startSq rest color
    code
      | color == White && code <= 6 -> checkPawnCaptures board startSq rest color
      | color == Black && code >= 7 -> checkPawnCaptures board startSq rest color
      | otherwise -> 
        if color == White && row == 0
        then [moveStr ++ "N", moveStr ++ "B", moveStr ++ "R", moveStr ++ "Q"] ++ checkPawnCaptures board startSq rest color
        else if color == Black && row == 7
             then [moveStr ++ "n", moveStr ++ "b", moveStr ++ "r", moveStr ++ "q"] ++ checkPawnCaptures board startSq rest color
             else moveStr : checkPawnCaptures board startSq rest color

-- different offset for white and black when pawn tries to move over piece
rowAdjustment :: Color -> Int -> Int
rowAdjustment (White) x = x + 1
rowAdjustment _  x = x - 1

-- simulates board after pseudo-legal move and checks if king is not in check
kingInCheck :: Board -> String -> Color -> Bool
kingInCheck board move color = 
  let boardAfterMove = playMove board move
      opponentColor = toggleColor color
      kingCoord = kingLocation (mapPieces boardAfterMove) color
      enemyMoves = listLegalMoves (mapPieces boardAfterMove) opponentColor boardAfterMove
  in elem kingCoord (map (snd . splitMove) enemyMoves)

kingLocation :: [((Int, Int), Int)] -> Color -> String
kingLocation [] _ = ""
kingLocation (((row, col), piece):rest) color = 
  if (piece == 6 && color == White) || (piece == 12 && color == Black) 
  then coordToSquare (row, col)
  else kingLocation rest color

checkmateDetection :: Board -> Color -> Bool
checkmateDetection board color  = 
  let kingCoord = kingLocation (mapPieces board) color
      opponentColor = toggleColor color
      enemyMoves = listLegalMoves (mapPieces board) opponentColor board
  in (elem kingCoord (map (snd . splitMove) enemyMoves))