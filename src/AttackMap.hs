module AttackMap (boardToAttackMap) where

import Types (Board, Color(..))
import Utils (checkBounds)

import qualified Data.Vector as V

boardToAttackMap :: Board -> Color -> V.Vector Bool
boardToAttackMap board color =
  let initialAttackMap = V.replicate 64 False
      attackedIndices = V.ifoldl' (\acc idx _ -> acc ++ attackedSquares board idx color) [] board
      updates = [(i, True) | i <- attackedIndices]
  in V.unsafeUpd initialAttackMap updates

attackedSquares :: Board -> Int -> Color -> [Int]
attackedSquares board idx color = 
  let piece = board V.! idx
      row = div idx 8
      col = mod idx 8
  in if color == White
     then case piece of
       1 -> pawnAttacks board (row, col) color
       2 -> knightAttacks board (row, col) color
       3 -> bishopAttacks board (row, col) color
       4 -> rookAttacks board (row, col) color
       5 -> queenAttacks board (row, col) color
       _ -> []
     else case piece of
       7  -> pawnAttacks board (row, col) color
       8  -> knightAttacks board (row, col) color
       9  -> bishopAttacks board (row, col) color
       10 -> rookAttacks board (row, col) color
       11 -> queenAttacks board (row, col) color
       _  -> []

pawnAttacks :: Board -> (Int, Int) -> Color -> [Int]
pawnAttacks board (row, col) color = 
  let pawnCaptures = [(row + x, col + y) 
                     | (x, y) <- if color == White then [(-1, 1), (-1, -1)] else [(1, -1), (1, 1)]
                     , checkBounds (row + x , col + y)]
  in checkPawnAttacks board (row, col) pawnCaptures color

knightAttacks :: Board -> (Int, Int) -> Color -> [Int]
knightAttacks board (row, col) color = 
  let moves = [(2, 1), (2, -1), (-2, 1), (-2, -1), (1, 2), (1, -2), (-1, 2), (-1, -2)]
      knigthMoves = [(row + x, col + y) | (x, y) <- moves, checkBounds (row + x, col + y)]
  in checkKnightAttacks board (row, col) knigthMoves color

bishopAttacks :: Board -> (Int, Int) -> Color -> [Int]
bishopAttacks board coord color =
  attacksInDirection board coord coord (1, 1) color
  ++ attacksInDirection board coord coord (1, -1) color
  ++ attacksInDirection board coord coord (-1, 1) color
  ++ attacksInDirection board coord coord (-1, -1) color

rookAttacks :: Board -> (Int, Int) -> Color -> [Int]
rookAttacks board coord color =
  attacksInDirection board coord coord (0, 1) color
  ++ attacksInDirection board coord coord (0, -1) color
  ++ attacksInDirection board coord coord (1, 0) color
  ++ attacksInDirection board coord coord (-1, 0) color

queenAttacks :: Board -> (Int, Int) -> Color -> [Int]
queenAttacks board coord color =
  attacksInDirection board coord coord (0, 1) color
  ++ attacksInDirection board coord coord (0, -1) color
  ++ attacksInDirection board coord coord (1, 0) color
  ++ attacksInDirection board coord coord (-1, 0) color
  ++ attacksInDirection board coord coord (1, 1) color
  ++ attacksInDirection board coord coord (1, -1) color
  ++ attacksInDirection board coord coord (-1, 1) color
  ++ attacksInDirection board coord coord (-1, -1) color


-- generates attack squares for sliding pieces
attacksInDirection :: Board -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Color -> [Int]
attacksInDirection board startSq (row, col) (x, y) color = 
  if checkBounds (row + x, col + y)
  then 
    let nextSq = V.unsafeIndex board ((row + x) * 8 + (col + y))
    in case nextSq of
         0 -> (row + x) * 8 + (col + y) : attacksInDirection board startSq (row + x, col + y) (x, y) color
         code
           | color == White && code <= 6 -> []
           | color == Black && code >= 7 -> []
           | otherwise -> 
              [(row + x) * 8 + (col + y)]
  else []

-- generates attack squares for knight
checkKnightAttacks :: Board -> (Int, Int) -> [(Int, Int)] -> Color -> [Int]
checkKnightAttacks _ _ [] _ = []
checkKnightAttacks board startSq ((row, col):rest) color = 
  case V.unsafeIndex board (row * 8 + col) of
    0 -> (row * 8 + col) : checkKnightAttacks board startSq rest color
    code
      | color == White && code <= 6 -> checkKnightAttacks board startSq rest color
      | color == Black && code >= 7 -> checkKnightAttacks board startSq rest color
      | otherwise -> 
        (row * 8 + col) : checkKnightAttacks board startSq rest color
   
-- generates attack squares for pawns
checkPawnAttacks :: Board -> (Int, Int) -> [(Int, Int)] -> Color -> [Int]
checkPawnAttacks _ _ [] _ = [] 
checkPawnAttacks board startSq ((row, col):rest) color = 
  case V.unsafeIndex board (row * 8 + col) of
    0 ->
      (row * 8 + col) : checkPawnAttacks board startSq rest color
    code
      | color == White && code <= 6 -> checkPawnAttacks board startSq rest color
      | color == Black && code >= 7 -> checkPawnAttacks board startSq rest color
      | otherwise -> (row * 8 + col) : checkPawnAttacks board startSq rest color