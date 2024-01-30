module Utils
  ( (!!!)
  , Milliseconds
  , Seconds
  , forceArray
  , forceJust
  , forceRight
  , popup
  , prettifyJSON
  , timeMS
  , timeSec
  )
  where

import Prelude

import Data.Array (unsafeIndex)
import Data.Either (Either(..), hush)
import Data.Int (round)
import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type Milliseconds = Number
type Seconds = Int

foreign import popup :: String -> Effect Unit

foreign import prettifyJSON :: String -> String

foreign import timeMS :: Effect Milliseconds

foreign import timeSec :: Effect Seconds

forceArray :: forall a. Array a -> Int -> a
forceArray a i = unsafePartial $ unsafeIndex a i

forceJust :: forall a. Maybe a -> a
forceJust m = unsafePartial $ fromJust m

forceRight :: forall a b. Either a b -> b
forceRight = forceJust <<< hush

infixl 8 forceArray as !!!