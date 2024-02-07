module Constants
  ( correctGreen
  , secondsPerDay
  , spacedRepetitionSchedule
  , tooSlowOrange
  , wrongMoveRed
  )
  where

import Prelude

correctGreen :: String
correctGreen = "#ACCE59"

secondsPerDay :: Int
secondsPerDay = 60 * 60 * 24

spacedRepetitionSchedule :: Array Number
spacedRepetitionSchedule = [0.01, 0.5, 6.0, 14.0, 30.0, 66.0, 150.0, 360.0]

tooSlowOrange :: String
tooSlowOrange = "#E6912C"

wrongMoveRed :: String
wrongMoveRed = "#F42A32"