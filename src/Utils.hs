module Utils (coordToSquare, squareToCoord, splitMove, checkBounds, toggleColor) where

import Types (Color(..))
import Data.Char

-- convert coordinate to move string
coordToSquare :: (Int, Int) -> String
coordToSquare (row, 0) = "a" ++ show (8 - row)
coordToSquare (row, 1) = "b" ++ show (8 - row)
coordToSquare (row, 2) = "c" ++ show (8 - row)
coordToSquare (row, 3) = "d" ++ show (8 - row)
coordToSquare (row, 4) = "e" ++ show (8 - row)
coordToSquare (row, 5) = "f" ++ show (8 - row)
coordToSquare (row, 6) = "g" ++ show (8 - row)
coordToSquare (row, 7) = "h" ++ show (8 - row)
coordToSquare _        = error "invalid coord"

-- convert square to coordinate
squareToCoord :: String -> (Int, Int)
squareToCoord square = 
  let col = ord (head square) - ord 'a'
      row = 8 - digitToInt (last square)
  in (row, col)

-- split move string to source square and destination square
splitMove :: String -> (String, String)
splitMove [a, b, c, d]    = ([a, b], [c, d])
splitMove [a, b, c, d, _] = ([a, b], [c, d])
splitMove _               = error "invalid move string"

-- check if move stays on the board
checkBounds :: (Int, Int) -> Bool
checkBounds (row, col) = 
  row >= 0 && row < 8 && col >= 0 && col < 8

-- toggle color
toggleColor :: Color -> Color
toggleColor White = Black
toggleColor _     = White