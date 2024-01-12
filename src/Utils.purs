module Utils
  ( (!!!)
  , forceArray
  , popup
  )
  where

import Prelude

import Data.Array (unsafeIndex)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

foreign import popup :: String -> Effect Unit

forceArray :: forall a. Array a -> Int -> a
forceArray a i = unsafePartial $ unsafeIndex a i

infixl 8 forceArray as !!!