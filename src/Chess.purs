module Chess
  ( FEN
  , Move
  , Orientation
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
type Orientation = String -- "white", "black"
type Action = { move :: Move, fen :: FEN }

foreign import setUpBoardAndWaitForMove_ :: FEN -> Orientation -> Effect (Promise Action)

foreign import fenAfterMove :: Move -> FEN -> FEN

setUpBoardAndWaitForMove :: FEN -> Orientation -> Aff Action
setUpBoardAndWaitForMove f o = toAffE $ setUpBoardAndWaitForMove_ f o