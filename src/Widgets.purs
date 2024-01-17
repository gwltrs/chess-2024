module Widgets
  ( MainMenuAction(..)
  , chessboard
  , fileMenu
  , mainMenu
  , mainMenuInputs
  , newPuzzle
  , root
  , textFieldsAndButton
  )
  where

import Prelude

import Chess (Color, FEN, Move, Move', destroyBoard, fenIsValid, sanitizeFEN, setUpBoardAndWaitForMove, turnFromFEN)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (br')
import Concur.React.DOM as D
import Concur.React.Props (placeholder)
import Concur.React.Props as P
import Control.Alt ((<|>))
import Data.Array (length, unsafeIndex, zip)
import Data.Either (Either(..))
import Data.Int (odd, round, toNumber)
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
import State (Puzzle, State)
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

button :: String -> Widget HTML Unit
button text = button' text true

button' :: String -> Boolean -> Widget HTML Unit
button' text enabled = void $ D.button 
  [ P.disabled (not enabled)
  , P.onClick
  , P.className "menuButton"
  ] 
  [D.text text]

chessboard :: FEN -> Color -> Widget HTML Move'
chessboard fen orient = board <|> setUp
  where 
    board = { fen: "", move: "" } <$ (D.div [P._id "board1", style] [])
    setUp = liftAff $ setUpBoardAndWaitForMove fen orient
    style = P.style { width: "400pt" }

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
    ReviewPuzzles -> mainMenu state

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
        , AddMove <$> chessboard fen' orientation
        ]
      case action of
        CancelPuzzle -> pure Nothing
        AddMove m -> inner m.fen (line <> [m.move])
        SavePuzzle -> do
          ts <- liftEffect timestamp
          pure $ Just 
            { name: name
            , fen: fen
            , line: line 
            , sr: Just { box: 1, lastReview: ts }
            }
  in
    inner fen []

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