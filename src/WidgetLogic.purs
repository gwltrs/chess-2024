module WidgetLogic
  ( puzzlesToReview
  , validateNewPuzzle
  )
  where

import Prelude

import Chess (FEN, fenIsValid, sanitizeFEN)
import Constants (secondsPerDay)
import Data.Array (elem, filter, mapMaybe, sortWith)
import Data.Either (Either(..))
import Data.Int (pow)
import Data.Maybe (Maybe)
import Data.String (length, trim)
import Data.Tuple (Tuple(..), fst, snd)
import State (Puzzle, Puzzle', fromPuzzle', reviewAfter, toPuzzle')

puzzlesToReview :: Int -> Array Puzzle -> Array Puzzle'
puzzlesToReview now puzzles = puzzles
    # mapMaybe toPuzzle'
    <#> (\p -> Tuple (reviewAfter p.sr) p)
    # filter (\t -> now > fst t)
    # sortWith (negate <<< fst)
    <#> snd

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