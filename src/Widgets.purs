module Widgets
  ( MainMenuAction(..)
  , chessboard
  , fileMenu
  , mainMenu
  , mainMenuInputs
  , newPuzzle
  , root
  , textFields
  )
  where

import Prelude

import Chess (Color, FEN, Move, Move', destroyBoard, setUpBoardAndWaitForMove, turnFromFEN)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (placeholder)
import Concur.React.Props as P
import Control.Alt ((<|>))
import Data.Array (unsafeIndex, zip)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Traversable (sequence)
import Data.Tuple (Tuple, uncurry)
import Data.Unfoldable (fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import React.Ref as R
import Unsafe.Coerce (unsafeCoerce)
import Utils ((!!!))

data MainMenuAction
  = NewPuzzle String FEN

data NewPuzzleAction
  = CancelPuzzle
  | AddMove Move'
  | SavePuzzle

type Puzzle = 
  { name :: String
  , fen :: FEN 
  , line :: Array Move
  }

type State =
  { puzzles :: Array Puzzle
  }

button :: String -> Widget HTML Unit
button text = void $ D.button [P.onClick, P.className "menuButton"] [D.text text]

chessboard :: FEN -> Color -> Widget HTML Move'
chessboard fen orient = board <|> setUp
  where 
    board = { fen: "", move: "" } <$ (D.div [P._id "board1", style] [])
    setUp = liftAff $ setUpBoardAndWaitForMove fen orient
    style = P.style { width: "400pt" }

fileMenu :: Widget HTML State
fileMenu = D.div' [{ puzzles: [] } <$ button "New"]

label :: forall a. String -> Widget HTML a
label text = D.input
  [ P.className "label"
  , P.readOnly true
  , P.value text
  , P.style { width: w <> "pt" }
  ]
    where w = show (50 + (7 * (length text)))

mainMenu :: State -> Widget HTML State
mainMenu state = do
  action <- mainMenuInputs
  case action of
    NewPuzzle name fen -> do
      puzzle <- newPuzzle name fen
      mainMenu (state { puzzles = state.puzzles <> fromMaybe puzzle })

mainMenuInputs :: Widget HTML MainMenuAction
mainMenuInputs  = do
  liftEffect destroyBoard
  D.div' 
    [ textFields ["Name", "FEN"] "New Puzzle" <#>
      (\a -> NewPuzzle (a !!! 0) (a !!! 1))
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
        , SavePuzzle <$ button "Save"
        , AddMove <$> chessboard fen' orientation
        ]
      case action of
        CancelPuzzle -> pure Nothing
        AddMove m -> inner m.fen (line <> [m.move])
        SavePuzzle -> pure $ Just { name: name, fen: fen, line: line }
  in
    inner fen []

root :: Widget HTML Unit
root = do
  file <- fileMenu
  void $ mainMenu file

textFields :: Array String -> String -> Widget HTML (Array String)
textFields placeholders buttonText = 
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