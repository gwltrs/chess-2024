module JSON where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Simple.JSON (readJSON_)
import State (Puzzle, State)

parseState :: String -> Maybe State
parseState = readJSON_

serializeArray :: Array String -> String
serializeArray a = "[" <> (joinWith "," a) <> "]"

serializePuzzle :: Puzzle -> String
serializePuzzle p = 
    "{" <>
    "\"name\":\"" <> p.name <> "\"," <>
    "\"fen\":\"" <> p.fen <> "\"," <>
    "\"line\":" <> (serializeArray $ wrapString <$> p.line) <> "" <>
    "}"

serializeState :: State -> String
serializeState s = 
    let puzzlesStr = serializeArray $ serializePuzzle <$> s.puzzles
    in "{\"puzzles\":" <> puzzlesStr <> "}"

wrapString :: String -> String
wrapString s = "\"" <> s <> "\""