module State
  ( Puzzle
  , Puzzle'
  , SpacedRepetition
  , State
  , allottedSeconds
  , fromPuzzle'
  , reviewAfter
  , toPuzzle'
  , updatePuzzle
  , updateSR
  )
  where

import Prelude

import Chess (FEN, Move)
import Constants (secondsPerDay, spacedRepetitionSchedule)
import Data.Array (length)
import Data.Int (pow, round, toNumber)
import Data.Maybe (Maybe(..))
import Utils (Milliseconds, Seconds, (!!!))

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
  , lastReview :: Int 
  }

type State =
  { puzzles :: Array Puzzle
  }

allottedSeconds :: Puzzle' -> Seconds
allottedSeconds p = 3 + (2 * numMoves) + (1 * p.sr.box)
  where 
    numMoves = ((length p.line - 1) / 2) + 1
  
fromPuzzle' :: Puzzle' -> Puzzle
fromPuzzle' p' = { name: p'.name, fen: p'.fen, line: p'.line, sr: Just p'.sr }


toPuzzle' :: Puzzle -> Maybe Puzzle'
toPuzzle' p = p.sr 
  <#> (\sr -> { name: p.name, fen: p.fen, line: p.line, sr: sr })

reviewAfter :: SpacedRepetition -> Seconds
reviewAfter sr = round (toNumber sr.lastReview + (toNumber secondsPerDay * (spacedRepetitionSchedule !!! sr.box)))

updateSR :: Seconds -> Boolean -> SpacedRepetition -> Maybe SpacedRepetition
updateSR now success sr =
  if not success then
    Just { box: 0, lastReview: now }
  else if sr.box == (length spacedRepetitionSchedule - 1) then
    Nothing
  else
    Just { box: sr.box + 1, lastReview: now }

updatePuzzle :: Seconds -> Boolean -> Puzzle' -> Puzzle
updatePuzzle now success puzzle = puzzle { sr = updateSR now success puzzle.sr }