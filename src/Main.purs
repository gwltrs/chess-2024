module Main where

import Prelude

import Chess (fenAfterMove, setUpBoardAndWaitForMove)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Types (FEN, Move, Move', Orientation, Puzzle)
import Utils ((!!!))
import Widgets (chessboard1, chessboard2, mainMenu, textInputs)

root :: Widget HTML Unit
root = do
  strs <- textInputs ["", ""] "just some strings"
  liftEffect $ log $ show strs
  _ <- chessboard1 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  _ <- mainMenu
  _ <- chessboard1 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  _ <- chessboard2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  _ <- chessboard1 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  _ <- chessboard2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  _ <- chessboard1 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  _ <- chessboard2 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  _ <- mainMenu
  _ <- chessboard1 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  void $ chessboard1 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white"
  

main :: Effect Unit
main = do
  runWidgetInDom "root" root
  --runWidgetInDom "root" (counterWidget 0)
  --log "🍝"