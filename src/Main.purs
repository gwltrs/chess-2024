module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Widgets (root)

main :: Effect Unit
main = runWidgetInDom "root" root