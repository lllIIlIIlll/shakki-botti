module PieceSquareTables (
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
) where

import qualified Data.Vector as V

whitePawnPrefCoords :: V.Vector Int
whitePawnPrefCoords = V.fromList [
  0,  0,  0,  0,  0,  0,  0,  0,
  90, 90, 90, 100, 100, 90, 90, 90,
  30, 30, 40, 60, 60, 40, 30, 30,
  10, 10, 20, 40, 40, 20, 10, 10,
  5,  5, 10, 30, 30, 10,  5,  5,
  0,  0,  0, 10, 10,  0,  0,  0,
  5,  5,  5,  0,  0,  5,  5,  5,
  0,  0,  0,  0,  0,  0,  0,  0
  ]

blackPawnPrefCoords :: V.Vector Int
blackPawnPrefCoords = V.map negate (V.reverse whitePawnPrefCoords)

whiteKnightPrefCoords :: V.Vector Int
whiteKnightPrefCoords = V.fromList  [
  -50,-40,-30,-30,-30,-30,-40,-50,
  -40,-20,  0,  5,  5,  0,-20,-40,
  -30,  5, 10, 15, 15, 10,  5,-30,
  -30,  5, 15, 20, 20, 15,  5,-30,
  -30,  5, 15, 20, 20, 15,  5,-30,
  -30,  5, 10, 15, 15, 10,  5,-30,
  -40,-20,  0,  0,  0,  0,-20,-40,
  -50,-40,-30,-30,-30,-30,-40,-50
  ]

blackKnightPrefCoords :: V.Vector Int
blackKnightPrefCoords = V.map negate whiteKnightPrefCoords

whiteBishopPrefCoords :: V.Vector Int
whiteBishopPrefCoords = V.fromList [
  -20,-10,-10,-10,-10,-10,-10,-20,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -10,  0,  5, 10, 10,  5,  0,-10,
  -10,  5,  5, 10, 10,  5,  5,-10,
  -10,  0, 10, 15, 15, 10,  0,-10,
  -10, 10, 10, 10, 10, 10, 10,-10,
  -10, 20,  0,  0,  0,  0, 20, -10,
  -20,-10,-10,-10,-10,-10,-10,-20
  ]

blackBishopPrefCoords :: V.Vector Int
blackBishopPrefCoords = V.map negate (V.reverse whiteBishopPrefCoords)

whiteRookPrefCoords :: V.Vector Int
whiteRookPrefCoords = V.fromList [
  0,  0,  0,  0,  0,  0,  0,  0,
  5, 20, 20, 20, 20, 20, 20,  5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  0,  0,  0,  15, 15, 0,  0,  0
  ]

blackRookPrefCoords :: V.Vector Int
blackRookPrefCoords = V.map negate (V.reverse whiteRookPrefCoords)

whiteQueenPrefCoords :: V.Vector Int
whiteQueenPrefCoords = V.fromList [
  -20,-10,-10, -5, -5,-10,-10,-20,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -10,  0,  10, 10, 10,  5, 0,-10,
  -5,   0, 10, 20, 20, 10, 0, -5,
  -5,   0, 10, 20, 20, 10, 0, -5,
  -10,  0,  10, 10, 10, 5,  0,-10,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -20,-10,-10, -5, -5,-10,-10,-20
  ]

blackQueenPrefCoords :: V.Vector Int
blackQueenPrefCoords = V.map negate (V.reverse whiteQueenPrefCoords)

{-
whiteKingPrefCoords :: V.Vector Int
whiteKingPrefCoords = V.fromList [
  -50,-30,-30,-30,-30,-30,-30,-50,
  -30,-30,  0,  0,  0,  0,-30,-30,
  -30,-10, 20, 30, 30, 20,-10,-30,
  -30,-10, 30, 40, 40, 30,-10,-30,
  -30,-10, 30, 40, 40, 30,-10,-30,
  -30,-10, 20, 30, 30, 20,-10,-30,
  -30,-20,-10,  0,  0,-10,-20,-30,
  -50,-40,-30,-20,-20,-30,-40,-50
  ]

blackKingPrefCoords :: V.Vector Int
blackKingPrefCoords = V.map negate (V.reverse whiteKingPrefCoords)
-}