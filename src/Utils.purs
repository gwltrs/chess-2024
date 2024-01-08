module Utils
  ( (!!!)
  , forceArray
  )
  where

import Prelude

import Data.Array (unsafeIndex)
import Partial.Unsafe (unsafePartial)

forceArray :: forall a. Array a -> Int -> a
forceArray a i = unsafePartial $ unsafeIndex a i

infixl 8 forceArray as !!!