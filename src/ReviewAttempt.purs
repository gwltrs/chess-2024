module ReviewAttempt
  ( ReviewAttempt
  , ReviewAttempt'
  , ReviewAttemptOutcome
  , fromReviewAttempt
  , isCorrect
  , toHighlight
  , toReviewAttempt
  )
  where

import Prelude

import Chess (FEN, Hex, Move, Highlight)
import Constants (spacedRepetitionSchedule)
import Data.Array (length)
import Data.Int (toNumber)
import Data.String.CodeUnits (slice)
import State (Puzzle', allottedSeconds)
import Utils (Milliseconds)

data ReviewAttemptOutcome
  = Correct
  | TooSlow
  | WrongMove

type ReviewAttempt =
  { correct :: Boolean
  , fen :: FEN
  , move :: Move
  }

type ReviewAttempt' =
  { outcome :: ReviewAttemptOutcome
  , fen :: FEN
  , move :: Move
  }

fromReviewAttempt :: Milliseconds -> Milliseconds -> Puzzle' -> ReviewAttempt -> ReviewAttempt'
fromReviewAttempt start end p ra =
  { outcome: 
      if not ra.correct then
        WrongMove
      else if (end - start) > (1000.0 * (toNumber $ allottedSeconds p)) then
        TooSlow
      else 
        Correct
  , fen: ra.fen
  , move: ra.move
  }

isCorrect :: ReviewAttemptOutcome -> Boolean
isCorrect Correct = true
isCorrect TooSlow = false
isCorrect WrongMove = false

toHighlight :: ReviewAttempt' -> Highlight
toHighlight ra = { square: square, hex: hex }
  where
    square = slice 2 4 ra.move
    hex = case ra.outcome of
      Correct -> "#ACCE59"
      TooSlow -> "#E6912C"
      WrongMove -> "#F42A32"

toReviewAttempt :: ReviewAttempt' -> ReviewAttempt
toReviewAttempt ra = 
  { correct: isCorrect ra.outcome
  , fen: ra.fen
  , move: ra.move
  }