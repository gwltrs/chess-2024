module State where

import Prelude

import Chess (FEN, Move)

type Puzzle = 
  { name :: String
  , fen :: FEN 
  , line :: Array Move
  }

type State =
  { puzzles :: Array Puzzle
  }
