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

import Chess (Color, FEN, Move, Move', destroyBoard, fenIsValid, sanitizeFEN, setUpBoardAndMakeMove, setUpBoardAndWaitForMove, turnFromFEN)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (br')
import Concur.React.DOM as D
import Concur.React.Props (placeholder)
import Concur.React.Props as P
import Control.Alt ((<|>))
import Data.Array (length, unsafeIndex, zip)
import Data.Either (Either(..))
import Data.Int (even, odd, round, toNumber)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import File (loadFile, saveFile)
import JSON (parseState, serializeState)
import React.Ref as R
import State (Puzzle', State, Puzzle, fromPuzzle')
import Unsafe.Coerce (unsafeCoerce)
import Utils (popup, timestamp, (!!!))
import WidgetLogic (puzzlesToReview, validateNewPuzzle)

data FileMenuAction
  = LoadFile
  | NewFile

data MainMenuAction
  = NewPuzzle String FEN
  | SaveState
  | PrintState
  | ReviewPuzzles

data NewPuzzleAction
  = AddMove Move'
  | CancelPuzzle
  | SavePuzzle

data ReviewPuzzleAction
  = CancelReview
  | NextPuzzle
  | RetryReview
  | ReviewMove Move'

button :: String -> Widget HTML Unit
button text = button' text true

button' :: String -> Boolean -> Widget HTML Unit
button' text enabled = void $ D.button 
  [ P.disabled (not enabled)
  , P.onClick
  , P.className "menuButton"
  ] 
  [D.text text]

chessboardGetMove :: FEN -> Color -> Widget HTML Move'
chessboardGetMove fen orient = board <|> setUp
  where 
    board = { fen: "", move: "" } <$ (D.div [P._id "board1", P.style { width: "400pt" }] [])
    setUp = liftAff $ setUpBoardAndWaitForMove fen orient

chessboardMakeMove :: FEN -> Color -> Move -> Widget HTML FEN
chessboardMakeMove fen orient move = board <|> setUp
  where
    board = D.div [P._id "board1", P.style { width: "400pt" }] []
    setUp = liftAff $ setUpBoardAndMakeMove fen orient move

chessboardShow :: FEN -> Color -> Widget HTML Unit
chessboardShow fen orient = pure unit

fileMenu :: Widget HTML State
fileMenu = do
  action <- D.div' 
    [ NewFile <$ button "New"
    , LoadFile <$ button "Load"
    ]
  case action of
    NewFile -> pure { puzzles: [] }
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

label :: forall a. String -> Widget HTML a
label text = D.input
  [ P.className "label"
  , P.readOnly true
  , P.value text
  , P.style { width: w <> "pt" }
  ]
    where w = show $ round (50.0 + (6.65 * (toNumber $ S.length text)))

mainMenu :: State -> Widget HTML State
mainMenu state = do
  now <- liftEffect timestamp
  action <- mainMenuInputs (length $ puzzlesToReview now state.puzzles)
  case action of
    NewPuzzle name fen -> do
      case validateNewPuzzle state.puzzles name fen of
        Left errMsg -> do
          liftEffect $ popup errMsg
          mainMenu state
        Right (Tuple name' fen') -> do
          puzzle <- newPuzzle name' fen'
          mainMenu (state { puzzles = state.puzzles <> fromMaybe puzzle })
    SaveState -> do
      liftEffect $ saveFile "data.txt" (serializeState state)
      mainMenu state
    PrintState -> do
      liftEffect $ log $ show state
      mainMenu state
    ReviewPuzzles -> do
      _ <- reviewPuzzle (puzzlesToReview now state.puzzles !!! 0)
      mainMenu state

mainMenuInputs :: Int -> Widget HTML MainMenuAction
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
    , br'
    , PrintState <$ button "Print State"
    ] 

newPuzzle :: String -> FEN -> Widget HTML (Maybe Puzzle)
newPuzzle name fen = 
  let 
    orientation = turnFromFEN fen
    inner :: FEN -> Array Move -> Widget HTML (Maybe Puzzle)
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
          ts <- liftEffect timestamp
          pure $ Just 
            { name: name
            , fen: fen
            , line: line 
            , sr: Just { box: 0, lastReview: ts }
            }
  in
    inner fen []

reviewPuzzle :: Puzzle' -> Widget HTML Boolean
reviewPuzzle puzzle = 
  let
    orient :: Color
    orient = turnFromFEN puzzle.fen
    inner :: FEN -> Int -> Widget HTML Boolean
    inner fen moveIndex = 
      if even moveIndex then do
        m <- chessboardGetMove fen orient
        if m.move == (puzzle.line !!! moveIndex) then 
          if moveIndex == (length puzzle.line - 1) then
            pure true
          else 
            inner m.fen (moveIndex + 1) 
        else
          pure false
      else do
        newFEN <- chessboardMakeMove fen orient (puzzle.line !!! moveIndex)
        inner newFEN (moveIndex + 1)
  in
    inner puzzle.fen 0

-- reviewPuzzle :: Puzzle' -> Widget HTML Puzzle
-- reviewPuzzle puzzle = 
--   let 
--     inner :: Boolean -> Int -> Widget HTML Puzzle
--     inner firstTry step = do
--       action <- D.div' 
--         [ CancelReview <$ button "Back"
--         , label puzzle.name
--         , RetryReview <$ button "Retry"
--         , NextPuzzle <$ button "Next"
--         ]
--       case action of
--         CancelReview -> pure $ fromPuzzle' puzzle
--         NextPuzzle -> pure $ fromPuzzle' puzzle
--         RetryReview -> pure $ fromPuzzle' puzzle
--         ReviewMove m -> pure $ fromPuzzle' puzzle
--   in
--     inner true 0

root :: Widget HTML Unit
root = do
  file <- fileMenu
  void $ mainMenu file

textFieldsAndButton :: Array String -> String -> Widget HTML (Array String)
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