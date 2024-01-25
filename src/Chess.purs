module Chess
  ( Color
  , FEN
  , Move
  , Move'
  , Square
  , destroyBoard
  , fenAfterMove
  , fenIsValid
  , sanitizeFEN
  , setUpBoardAndDoNothing
  , setUpBoardAndMakeMove
  , setUpBoardAndWaitForMove
  , turnFromFEN
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

type Color = String -- "white", "black"
type FEN = String -- "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -" (w/0 move #s)
type Move = String -- "e2e4", "f7f8n", "f7f8b", "f7f8r"
type Move' = { move :: Move, fen :: FEN }
type Square = String -- "e4", "a1", "h8"

foreign import destroyBoard :: Effect Unit

foreign import fenAfterMove :: Move -> FEN -> FEN

foreign import fenIsValid :: FEN -> Boolean

foreign import sanitizeFEN :: FEN -> FEN

foreign import setUpBoardAndDoNothing_ :: FEN -> Color -> Effect (Promise Void)

foreign import setUpBoardAndMakeMove_ :: FEN -> Color -> Move -> Effect (Promise FEN)

foreign import setUpBoardAndWaitForMove_ :: FEN -> Color -> Effect (Promise Move')

foreign import turnFromFEN :: FEN -> Color

setUpBoardAndDoNothing :: forall a. FEN -> Color -> Aff a
setUpBoardAndDoNothing f o = absurd <$> (toAffE $ setUpBoardAndDoNothing_ f o)

setUpBoardAndMakeMove :: FEN -> Color -> Move -> Aff FEN
setUpBoardAndMakeMove f o m = toAffE $ setUpBoardAndMakeMove_ f o m

setUpBoardAndWaitForMove :: FEN -> Color -> Aff Move'
setUpBoardAndWaitForMove f o = toAffE $ setUpBoardAndWaitForMove_ f o