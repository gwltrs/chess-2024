module WidgetLogic
  ( incrementName
  , puzzlesToReview
  , validateNewPuzzle
  )
  where

import Prelude

import Chess (FEN, fenIsValid, sanitizeFEN)
import Data.Array (elem, filter, findIndex, mapMaybe, sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (length, trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (multiline)
import Data.Tuple (Tuple(..), fst, snd)
import State (Puzzle, Puzzle', reviewAfter, toPuzzle')
import Utils (forceRight)

-- Auto-incrementing the name removes the hassle of duplicate names
-- "endgame" -> "endgame" if no puzzles already exist
-- "endgame" -> "endgame #2" if one puzzle already exists with the names "endgame"
-- "endgame" -> "endgame #3" if two puzzles already exist with the names "endgame" and "endgame #2"
-- Behavior is undefined for untrimmed names
incrementName :: Array Puzzle -> String -> String
incrementName puzzles name = 
    if isNothing (findIndex (\p -> p.name == name) puzzles) then
        name
    else let
        previousIncrementsRegex :: Regex
        previousIncrementsRegex = forceRight $ regex "^(\\S.*\\S)\\s+#([1-9][0-9]*)$" multiline
        currentIncrement :: Int
        currentIncrement = puzzles
            <#> _.name 
            # (mapMaybe (match previousIncrementsRegex))
            # (mapMaybe \nonEmptyArr -> 
                case toArray nonEmptyArr of
                [Just _, Just firstCapture, Just intString] -> 
                    fromString intString <#> Tuple firstCapture
                _ ->
                    Nothing)
            # filter (\tup -> fst tup == name)
            <#> snd
            # (foldr max 1)
            # add 1
        in
            name <> " #" <> show currentIncrement

puzzlesToReview :: Int -> Array Puzzle -> Array Puzzle'
puzzlesToReview now puzzles = puzzles
    # mapMaybe toPuzzle'
    <#> (\p -> Tuple (reviewAfter p.sr) p)
    # filter (\t -> now > fst t)
    # sortWith fst
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
            let fen'' = sanitizeFEN fen' in
            if elem fen'' (_.fen <$> puzzles) then
                Left "Duplicate FEN"
            else
                Right (Tuple (incrementName puzzles name') fen'')