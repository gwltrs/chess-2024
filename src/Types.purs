module Types
  ( FEN
  , Move
  , Move'
  , Orientation
  , Puzzle
  )
  where

type FEN = String -- "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -" (w/0 move #s)
type Move = String -- "e2e4", "f7f8n", "f7f8b", "f7f8r"
type Move' = { move :: Move, fen :: FEN }
type Orientation = String -- "white", "black"
type Puzzle = 
    { name :: String
    , fen :: FEN 
    , line :: Array Move
    }