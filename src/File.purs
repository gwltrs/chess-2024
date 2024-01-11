module File
  ( loadFile
  , saveFile
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)

foreign import loadFile_ :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Effect (Promise (Maybe String))

foreign import saveFile :: String -> String -> Effect Unit

loadFile :: Aff (Maybe String)
loadFile = toAffE $ loadFile_ Just Nothing