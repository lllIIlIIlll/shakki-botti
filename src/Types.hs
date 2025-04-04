module Types (Color(..), Piece(..), Move(..), Board, GameState(..)) where

import Data.Vector as V

data Color = White | Black
  deriving (Show, Eq)

data Piece = Pawn | Rook | Knight | Bishop | King | Queen
  deriving (Show, Eq)

data GameState = GameState {
  board        :: Board,
  turn         :: Color,
  wKingCastle  :: Bool,
  wQueenCastle :: Bool,
  bKingCastle  :: Bool,
  bQueenCastle :: Bool,
  enPassant    :: Maybe (Int, Int),
  halfMove     :: Int,
  fullMove     :: Int
} deriving (Show, Eq)

type Board = V.Vector Int

-- 0 - empty sq
-- 1 - white pawn
-- 2 - white knight
-- 3 - white bishop
-- 4 - white rook
-- 5 - white queen
-- 6 - white king
-- 7 - black pawn
-- 8 - black knight
-- 9 - black bishop
-- 10 - black rook
-- 11 - black queen
-- 12 - black king

data Move = Move {
  from      :: (Int, Int),
  dest      :: (Int, Int),
  promotion :: Maybe Char,
  capture   :: Maybe Int,
  castle    :: Maybe ((Int, Int), (Int, Int))
} deriving (Show, Eq)