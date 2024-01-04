module Main where

import Prelude

import Chess (Move, FEN, Orientation, fenAfterMove, setUpBoardAndWaitForMove)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

-- counterWidget :: forall a. Int -> Widget HTML a
-- counterWidget count = do
--   n <- D.div'
--         [ D.p' [D.text ("State: " <> show count)]
--         , D.button [P.onClick] [D.text "Decrement"] $> count-1
--         , D.button [P.onClick] [D.text "Increment"] $> count+1
--         ]
--   liftEffect (log ("COUNT IS NOW: " <> show n))
--   counterWidget n

chessboard :: FEN -> Orientation -> Widget HTML Move
chessboard fen orient = do
  action <- board <|> setUp
  chessboard action.fen orient
  where 
    board = { fen: "", move: "" } <$ (D.div [P._id "board1", style] [])
    setUp = liftAff $ setUpBoardAndWaitForMove fen orient
    style = P.style 
      { width: "400px"
      }

main :: Effect Unit
main = do
  runWidgetInDom "root" (chessboard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" "white")
  --runWidgetInDom "root" (counterWidget 0)
  --log "🍝"