module Widgets
  ( MainMenuAction(..)
  , chessboardGetMove
  , fileMenu
  , mainMenu
  , mainMenuInputs
  , newPuzzle
  , reviewPuzzle
  , root
  , textFieldsAndButton
  )
  where

import Prelude

import Chess (Color, FEN, Highlight, Move, Move', Square, destroyBoard, fenIsValid, sanitizeFEN, setUpBoardAndDoNothing, setUpBoardAndMakeMove, setUpBoardAndWaitForMove, turnFromFEN)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (br')
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Data.Array (find, findIndex, head, length, null, sortWith, unsafeIndex, updateAt, zip)
import Data.Either (Either(..))
import Data.Int (even, odd, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as S
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable as U
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import File (loadFile, saveFile)
import JSON (parseState, serializeState)
import React.DOM.Dynamic (object)
import React.Ref as R
import ReviewAttempt (ReviewAttempt, ReviewAttempt', fromReviewAttempt, toHighlight, toHighlight')
import State (Puzzle, Puzzle', State, previousPuzzle, toPuzzle, updatePuzzle)
import Unsafe.Coerce (unsafeCoerce)
import Utils (Milliseconds, Seconds, forceArray, forceJust, popup, prettifyJSON, timeMS, timeSec, (!!!))
import WidgetLogic (puzzlesToReview, validateNewPuzzle)

data FileMenuAction
  = LoadFile
  | NewFile

data MainMenuAction
  = NewPuzzle String FEN
  | SaveState
  | PrintState
  | PracticePrevious
  | ReviewPuzzles

data NewPuzzleAction
  = AddMove Move'
  | CancelPuzzle
  | SavePuzzle
  
data PracticePuzzleAction
  = CancelPractice
  | RetryPractice
  | PracticeAttempted ReviewAttempt

data ReviewPuzzleAction
  = CancelReview
  | NextPuzzle
  | RetryReview
  | ReviewAttempted ReviewAttempt

type ReviewResult = 
  { puzzle :: Puzzle
  , continue :: Boolean
  }

type Widget' = Widget HTML

board :: forall a. Widget' a
board = D.div [P._id "board", P.style { width: "400pt" }] []

button :: String -> Widget' Unit
button text = button' text true

button' :: String -> Boolean -> Widget' Unit
button' text enabled = void $ D.button 
  [ P.disabled (not enabled)
  , P.onClick
  , P.className "menuButton"
  ] 
  [D.text text]

chessboardGetMove :: FEN -> Color -> Widget' Move'
chessboardGetMove fen orient = board <|> setUp
  where 
    setUp = liftAff $ setUpBoardAndWaitForMove fen orient

chessboardMakeMove :: FEN -> Color -> Move -> Widget' FEN
chessboardMakeMove fen orient move = board <|> setUp
  where
    setUp = liftAff $ setUpBoardAndMakeMove fen orient move

chessboardReviewPuzzle :: Puzzle -> Widget' ReviewAttempt
chessboardReviewPuzzle puzzle = 
  let
    orient :: Color
    orient = turnFromFEN puzzle.fen
    inner :: FEN -> Int -> Widget' ReviewAttempt
    inner fen moveIndex = 
      if even moveIndex then do
        m <- chessboardGetMove fen orient
        if m.move == (puzzle.line !!! moveIndex) then 
          if moveIndex == (length puzzle.line - 1) then
            pure { correct: true, fen: m.fen, move: m.move }
          else 
            inner m.fen (moveIndex + 1) 
        else
          pure { correct: false, fen: m.fen, move: m.move }
      else do
        newFEN <- chessboardMakeMove fen orient (puzzle.line !!! moveIndex)
        inner newFEN (moveIndex + 1)
  in
    inner puzzle.fen 0

chessboardShow :: forall a. FEN -> Color -> Maybe Highlight -> Widget' a
chessboardShow fen orient highlight = board <|> setUp
  where 
    setUp = liftAff $ setUpBoardAndDoNothing fen orient highlight

fileMenu :: Widget' State
fileMenu = do
  action <- D.div' 
    [ NewFile <$ button "New"
    , LoadFile <$ button "Load"
    ]
  case action of
    NewFile -> pure { previous: Nothing, puzzles: [] }
    LoadFile -> do
      fileTextMaybe <- liftAff loadFile 
        <|> (Nothing <$ fileMenu)
      case fileTextMaybe of
        Nothing -> fileMenu
        Just fileText ->
          case parseState fileText of
            Nothing -> do 
              liftEffect $ popup "Incorrect File Format"
              fileMenu
            Just state -> pure state

label :: forall a. String -> Widget' a
label text = D.input
  [ P.className "label"
  , P.readOnly true
  , P.value text
  , P.style { width: w <> "pt" }
  ]
    where w = show $ round (50.0 + (6.65 * (toNumber $ S.length text)))

mainMenu :: State -> Widget' State
mainMenu s = do
  puzzles' <- liftEffect $ flip puzzlesToReview s.puzzles <$> timeSec
  action <- mainMenuInputs (length puzzles')
  case action of
    NewPuzzle name fen -> do
      case validateNewPuzzle s.puzzles name fen of
        Left errMsg -> do
          liftEffect $ popup errMsg
          mainMenu s
        Right (Tuple name' fen') -> do
          puzzle <- newPuzzle name' fen'
          let newPuzzles = sortWith _.name (s.puzzles <> U.fromMaybe puzzle)
          mainMenu (s { puzzles = newPuzzles })
    PracticePrevious -> do
      fromMaybe (pure unit) (practicePuzzle <$> previousPuzzle s)
      mainMenu s
    PrintState -> do
      liftEffect $ log $ show s
      mainMenu s
    ReviewPuzzles -> do
      reviewedPuzzles <- reviewPuzzles s.puzzles
      mainMenu s { puzzles = reviewedPuzzles }
    SaveState -> do
      liftEffect $ saveFile "data.txt" (prettifyJSON $ serializeState s)
      mainMenu s

mainMenuInputs :: Int -> Widget' MainMenuAction
mainMenuInputs numPuzzles' = do
  liftEffect destroyBoard
  D.div' 
    [ SaveState <$ button "Save"
    , textFieldsAndButton ["Name", "FEN"] "New Puzzle" <#>
      (\a -> NewPuzzle (a !!! 0) (a !!! 1))
    , ReviewPuzzles <$ 
        if numPuzzles' > 0 
        then button ("Review (" <> show numPuzzles' <> ")")
        else button' "Review" false
    , PracticePrevious <$ button "Previous"
    , br'
    , PrintState <$ button "Print State"
    ] 

newPuzzle :: String -> FEN -> Widget' (Maybe Puzzle)
newPuzzle name fen = 
  let 
    orientation = turnFromFEN fen
    inner :: FEN -> Array Move -> Widget' (Maybe Puzzle)
    inner fen' line = do
      action <- D.div'
        [ CancelPuzzle <$ button "Back"
        , label name
        , SavePuzzle <$ button' "Save" (odd $ length line)
        , AddMove <$> chessboardGetMove fen' orientation
        ]
      case action of
        CancelPuzzle -> pure Nothing
        AddMove m -> do
          inner m.fen (line <> [m.move])
        SavePuzzle -> do
          t <- liftEffect timeSec
          pure $ Just 
            { name: name
            , fen: fen
            , line: line 
            , sr: Just { box: 0, lastReview: t }
            }
  in
    inner fen []

practicePuzzle :: Puzzle -> Widget' Unit
practicePuzzle p = 
  let
    inner :: Maybe ReviewAttempt -> Widget' Unit
    inner ra = do
      action <- D.div'
        [ CancelPractice <$ button "Back"
        , label p.name
        , RetryPractice <$ button "Retry"
        , case ra of
          Just ra' -> chessboardShow ra'.fen (turnFromFEN p.fen) (Just $ toHighlight ra')
          Nothing -> PracticeAttempted <$> chessboardReviewPuzzle p
        ]
      case action of
        CancelPractice -> pure unit
        RetryPractice -> inner Nothing
        PracticeAttempted ra -> inner $ Just ra
  in 
    inner Nothing

reviewPuzzle :: Puzzle' -> Widget' ReviewResult
reviewPuzzle puzzle = 
  let
    inner :: Milliseconds -> Maybe Boolean -> Maybe ReviewAttempt' -> Widget' ReviewResult
    inner start firstTry currentTry = do
      action <- D.div'
        [ CancelReview <$ button "Back"
        , label puzzle.name
        , RetryReview <$ button' "Retry" (isJust firstTry)
        , NextPuzzle <$ button' "Next" (isJust firstTry)
        , case currentTry of
            Just ra -> chessboardShow ra.fen (turnFromFEN puzzle.fen) (Just $ toHighlight' ra)
            Nothing -> ReviewAttempted <$> chessboardReviewPuzzle (toPuzzle puzzle)
        ]
      case action of
        CancelReview ->
          case firstTry of
            Nothing -> 
              pure { puzzle: toPuzzle puzzle, continue: false }
            Just correct -> do
              now <- liftEffect timeSec
              pure { puzzle: updatePuzzle now correct puzzle, continue: false }
        NextPuzzle -> do
          now <- liftEffect timeSec
          pure { puzzle: updatePuzzle now (forceJust firstTry) puzzle, continue: true }
        RetryReview -> do
          newStart <- liftEffect timeMS
          inner newStart firstTry Nothing
        ReviewAttempted attempt -> do
          end <- liftEffect timeMS
          inner start (Just $ fromMaybe attempt.correct firstTry) (Just $ fromReviewAttempt start end puzzle attempt)
  in do
    newStart <- liftEffect timeMS
    inner newStart Nothing Nothing

reviewPuzzles :: Array Puzzle -> Widget' (Array Puzzle)
reviewPuzzles puzzles = do
  puzzles' <- liftEffect $ flip puzzlesToReview puzzles <$> timeSec
  case head puzzles' of
    Nothing -> do
      liftEffect $ popup "No more puzzles to review"
      pure puzzles
    Just p' -> do
      let i = forceJust $ findIndex (\e -> e.name == p'.name || e.fen == p'.fen) puzzles
      a <- reviewPuzzle p'
      let newPuzzles = forceJust $ updateAt i a.puzzle puzzles 
      if a.continue then reviewPuzzles newPuzzles else pure newPuzzles
  
root :: Widget' Unit
root = do
  file <- fileMenu
  void $ mainMenu file

textFieldsAndButton :: Array String -> String -> Widget' (Array String)
textFieldsAndButton placeholders buttonText = 
  let
    tf placeholder ref = D.input 
      [ P.ref (R.fromRef ref)
      , P.className "textField"
      , P.placeholder placeholder
      ]
    tfValue i = do
      nodeMaybe <- R.getCurrentRef i
      case nodeMaybe of
        Nothing -> pure ""
        Just node -> pure (unsafeCoerce node).value
    bttn refs = do 
      _ <- D.button [P.onClick, P.className "menuButton"] [D.text buttonText] 
      liftEffect $ sequence (tfValue <$> refs)
  in do 
    refs :: Array (R.Ref R.NativeNode) <- liftEffect $ sequence $ (const R.createNodeRef) <$> placeholders
    D.div'
      ((zip placeholders refs 
        <#> (\t -> [] <$ (uncurry tf) t)) 
        <> [bttn refs])