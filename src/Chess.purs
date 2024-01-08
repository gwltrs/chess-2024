module Chess
  ( destroyBoard
  , fenAfterMove
  , setUpBoardAndWaitForMove
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Types (FEN, Move, Orientation, Move')

foreign import destroyBoard :: Effect Unit

foreign import setUpBoardAndWaitForMove_ :: FEN -> Orientation -> Effect (Promise Move')

foreign import fenAfterMove :: Move -> FEN -> FEN

setUpBoardAndWaitForMove :: FEN -> Orientation -> Aff Move'
setUpBoardAndWaitForMove f o = toAffE $ setUpBoardAndWaitForMove_ f o