module State
  ( Puzzle
  , Puzzle'
  , SpacedRepetition
  , State
  , Timestamp
  , fromPuzzle'
  , reviewAfter
  , toPuzzle'
  )
  where

import Prelude

import Chess (FEN, Move)
import Constants (boxExponent, secondsPerDay)
import Data.Int (pow)
import Data.Maybe (Maybe(..))

type Puzzle = 
  { name :: String
  , fen :: FEN 
  , line :: Array Move
  , sr :: Maybe SpacedRepetition
  }

type Puzzle' = 
  { name :: String
  , fen :: FEN 
  , line :: Array Move
  , sr :: SpacedRepetition
  }

type SpacedRepetition = 
  { box :: Int
  , lastReview :: Timestamp 
  }

type State =
  { puzzles :: Array Puzzle
  }

type Timestamp = Int

fromPuzzle' :: Puzzle' -> Puzzle
fromPuzzle' p' = { name: p'.name, fen: p'.fen, line: p'.line, sr: Just p'.sr }

toPuzzle' :: Puzzle -> Maybe Puzzle'
toPuzzle' p = p.sr 
  <#> (\sr -> { name: p.name, fen: p.fen, line: p.line, sr: sr })

reviewAfter :: SpacedRepetition -> Timestamp
reviewAfter sr = sr.lastReview + (secondsPerDay * (pow boxExponent (sr.box - 1)))