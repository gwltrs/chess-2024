module Widgets
  ( MainMenuAction(..)
  , chessboard1
  , chessboard2
  , mainMenu
  , textInputs
  )
  where

import Prelude

import Chess (setUpBoardAndWaitForMove)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Data.Array (unsafeIndex, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple, uncurry)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import React.Ref as R
import Types (FEN, Move, Move', Orientation, Puzzle)
import Unsafe.Coerce (unsafeCoerce)
import Utils ((!!!))

data MainMenuAction
    = NewPuzzle String FEN

button :: Widget HTML Unit
button = pure unit

chessboard1 :: FEN -> Orientation -> Widget HTML Move'
chessboard1 fen orient = board <|> setUp
  where 
    board = { fen: "", move: "" } <$ (D.div [P._id "board1", style] [])
    setUp = liftAff $ setUpBoardAndWaitForMove fen orient "board1"
    style = P.style { width: "400px" }

chessboard2 :: FEN -> Orientation -> Widget HTML Move'
chessboard2 fen orient = board <|> setUp
  where 
    board = { fen: "", move: "" } <$ (D.div [P._id "board2", style] [])
    setUp = liftAff $ setUpBoardAndWaitForMove fen orient "board2"
    style = P.style { width: "400px" }

mainMenu :: Widget HTML MainMenuAction
mainMenu = D.div' 
  [ textInputs ["name", "fen"] "new puzzle" <#>
      (\a -> NewPuzzle (a !!! 0) (a !!! 1))
  ] 

newPuzzle :: String -> FEN -> Widget HTML (Maybe Puzzle)
newPuzzle name fen = 
  let 
    inner :: String -> FEN -> Array Move -> Widget HTML (Maybe Puzzle)
    inner n f m = pure Nothing
  in
    pure Nothing

rows :: forall a. Array (Array (Widget HTML a)) -> Widget HTML a
rows arr = rows arr

textInputs :: Array String -> String -> Widget HTML (Array String)
textInputs placeholders buttonText = do 
  refs :: Array (R.Ref R.NativeNode) <- liftEffect $ sequence $ (const R.createNodeRef) <$> placeholders
  D.div'
    ((zip placeholders refs 
      <#> (\t -> [] <$ (uncurry tiLabel) t)) 
      <> [tiButton refs])
     
tiLabel :: String -> R.Ref R.NativeNode -> Widget HTML Unit
tiLabel placeholder ref = D.input [P.ref (R.fromRef ref)] 

inputValue :: R.Ref R.NativeNode -> Effect String
inputValue i = do
  nodeMaybe <- R.getCurrentRef i
  case nodeMaybe of
    Nothing -> pure ""
    Just node -> pure (unsafeCoerce node).value

tiButton :: Array (R.Ref R.NativeNode) -> Widget HTML (Array String)
tiButton refs = do 
  _ <- D.button [P.onClick] [D.text "buttonlabel"] 
  liftEffect $ sequence (inputValue <$> refs)