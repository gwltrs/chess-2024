module JSON where

import Prelude

import Data.Maybe (Maybe(..))
import Simple.JSON (readJSON_)
import State (State)

parseState :: String -> Maybe State
parseState = readJSON_

serializeState :: State -> String
serializeState state = ""