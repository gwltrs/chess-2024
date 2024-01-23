module Constants
  ( secondsPerDay
  , spacedRepetitionSchedule
  )
  where

import Prelude

secondsPerDay :: Int
secondsPerDay = 60 * 60 * 24

spacedRepetitionSchedule :: Array Number
spacedRepetitionSchedule = [0.5, 6.0, 14.0, 30.0, 66.0, 150.0, 360.0]