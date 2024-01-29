module Utils
  ( (!!!)
  , Milliseconds
  , Seconds
  , forceArray
  , forceJust
  , popup
  , timeMS
  , timeSec
  )
  where

import Prelude

import Data.Array (unsafeIndex)
import Data.Int (round)
import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

type Milliseconds = Number
type Seconds = Int

foreign import popup :: String -> Effect Unit

foreign import timeMS :: Effect Milliseconds

foreign import timeSec :: Effect Seconds

forceArray :: forall a. Array a -> Int -> a
forceArray a i = unsafePartial $ unsafeIndex a i

forceJust :: forall a. Maybe a -> a
forceJust m = unsafePartial $ fromJust m

infixl 8 forceArray as !!!