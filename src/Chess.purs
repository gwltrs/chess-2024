module Chess
  ( FEN
  , Move
  , Move'
  , Orientation
  , destroyBoard
  , fenAfterMove
  , setUpBoardAndWaitForMove
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)


type FEN = String -- "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -" (w/0 move #s)
type Move = String -- "e2e4", "f7f8n", "f7f8b", "f7f8r"
type Move' = { move :: Move, fen :: FEN }
type Orientation = String -- "white", "black"

foreign import destroyBoard :: Effect Unit

foreign import setUpBoardAndWaitForMove_ :: FEN -> Orientation -> Effect (Promise Move')

foreign import fenAfterMove :: Move -> FEN -> FEN

setUpBoardAndWaitForMove :: FEN -> Orientation -> Aff Move'
setUpBoardAndWaitForMove f o = toAffE $ setUpBoardAndWaitForMove_ f o