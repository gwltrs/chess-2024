module JSON where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Simple.JSON (readJSON_)
import State (Puzzle, State, SpacedRepetition)

parseState :: String -> Maybe State
parseState = readJSON_

serializeArray :: Array String -> String
serializeArray a = "[" <> (joinWith "," a) <> "]"

serializePuzzle :: Puzzle -> String
serializePuzzle p = 
    "{" <>
    "\"name\":\"" <> p.name <> "\"," <>
    "\"fen\":\"" <> p.fen <> "\"," <>
    "\"line\":" <> (serializeArray $ wrapString <$> p.line) <> "," <>
    "\"sr\":" <> (serializeSR p.sr) <> "" <>
    "}"

serializeSR :: Maybe SpacedRepetition -> String
serializeSR Nothing = "null"
serializeSR (Just sr) = 
    "{" <>
    "\"box\":" <> show sr.box <> "," <>
    "\"lastReview\":" <> show sr.lastReview <> "" <>
    "}"

serializeState :: State -> String
serializeState s = 
    let 
        puzzlesStr = serializeArray $ serializePuzzle <$> s.puzzles
        previousStr = case s.previous of
            Just p -> wrapString p
            Nothing -> "null"
    in 
        "{\"previous\":" <> previousStr <> ",\"puzzles\":" <> puzzlesStr <> "}"

wrapString :: String -> String
wrapString s = "\"" <> s <> "\""