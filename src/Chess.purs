module Chess
  ( fenAfterMove
  , setUpBoardAndWaitForMove
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Types (FEN, Move, Orientation, Move')

foreign import setUpBoardAndWaitForMove_ :: FEN -> Orientation -> String -> Effect (Promise Move')

foreign import fenAfterMove :: Move -> FEN -> FEN

setUpBoardAndWaitForMove :: FEN -> Orientation -> String -> Aff Move'
setUpBoardAndWaitForMove f o id = toAffE $ setUpBoardAndWaitForMove_ f o id