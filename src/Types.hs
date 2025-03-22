module Types (Color(..), Piece(..), Board) where

data Color = White | Black
  deriving (Show, Eq)

data Piece = Pawn | Rook | Knight | Bishop | King | Queen
  deriving (Show, Eq)

type Board = [[Maybe (Color, Piece)]]