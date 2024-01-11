module File where

import Prelude

import Effect (Effect)

foreign import saveFile :: String -> String -> Effect Unit