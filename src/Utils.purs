module Utils
  ( (!!!)
  , Timestamp
  , forceArray
  , forceJust
  , popup
  , timestamp
  )
  where

import Prelude

import Data.Array (unsafeIndex)
import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

type Timestamp = Int

foreign import popup :: String -> Effect Unit

foreign import timestamp :: Effect Timestamp

forceArray :: forall a. Array a -> Int -> a
forceArray a i = unsafePartial $ unsafeIndex a i

forceJust :: forall a. Maybe a -> a
forceJust m = unsafePartial $ fromJust m

infixl 8 forceArray as !!!