module WidgetLogic
  ( validateNewPuzzle
  )
  where

import Prelude

import Chess (FEN, fenIsValid, sanitizeFEN)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String (length, trim)
import Data.Tuple (Tuple(..))
import State (Puzzle)

validateNewPuzzle :: Array Puzzle -> String -> FEN -> Either String (Tuple String FEN)
validateNewPuzzle puzzles name fen = 
    let 
        name' = trim name
        fen' = trim fen
    in 
        if length name' == 0 then
            Left "Invalid Name" 
        else if not $ fenIsValid fen' then
            Left "Invalid FEN"
        else
            let fen'' = sanitizeFEN fen'
            in if elem name' (_.name <$> puzzles) then
                Left "Duplicate Name"
            else if elem fen'' (_.fen <$> puzzles) then
                Left "Duplicate FEN"
            else
                Right (Tuple name' fen'')