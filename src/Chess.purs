module Chess
  ( ChessboardMove
  , FEN
  , Orientation
  , Piece
  , Square
  , emptyMove
  , setUpBoardAndWaitForMove
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

type FEN = String -- "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k -" (w/0 move #s)
type Square = String -- "e4", "f7"
type Piece = String -- "wK", "bN"
type Orientation = String -- "white", "black"

type ChessboardMove = 
  { source :: Square
  , target :: Square
  , piece :: Piece
  , newPos :: FEN
  , oldPos :: FEN
  , orientation :: Orientation
  }

foreign import setUpBoardAndWaitForMove_ :: FEN -> Orientation -> Effect (Promise ChessboardMove)

setUpBoardAndWaitForMove :: FEN -> Orientation -> Aff ChessboardMove
setUpBoardAndWaitForMove f o = toAffE $ setUpBoardAndWaitForMove_ f o

emptyMove :: ChessboardMove
emptyMove = { source: "", target: "", piece: "", newPos: "", oldPos: "", orientation: "" }