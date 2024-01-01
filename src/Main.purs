module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- D.div'
        [ D.p' [D.text ("State: " <> show count)]
        , D.button [P.onClick] [D.text "Decrement"] $> count-1
        , D.button [P.onClick] [D.text "Increment"] $> count+1
        ]
  liftEffect (log ("COUNT IS NOW: " <> show n))
  counterWidget n

-- fetchWidget :: forall a. Widget HTML a
-- fetchWidget = do
--   today <- liftEffect getToday
--   D.text $ show today

main :: Effect Unit
main = do
  runWidgetInDom "root" (counterWidget 0)
  --log "🍝"

