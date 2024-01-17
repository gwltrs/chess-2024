module Utils
  ( (!!!)
  , forceArray
  , popup
  , timestamp
  )
  where

import Prelude

import Data.Array (unsafeIndex)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import State (Timestamp)

foreign import popup :: String -> Effect Unit

foreign import timestamp :: Effect Timestamp

forceArray :: forall a. Array a -> Int -> a
forceArray a i = unsafePartial $ unsafeIndex a i

infixl 8 forceArray as !!!