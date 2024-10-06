module Node.FS.Internal.Utils where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn2, mkEffectFn3)

type JSCallback0 = EffectFn1 (Nullable Error) Unit
type JSCallback1 a = EffectFn2 (Nullable Error) a Unit
type JSCallback2 a b = EffectFn3 (Nullable Error) a b Unit

-- | Type synonym for callback functions.
type Callback0 = Maybe Error -> Effect Unit
type Callback1 a = Either Error a -> Effect Unit

handleCallback1 :: forall a. Callback1 a -> JSCallback1 a
handleCallback1 cb = mkEffectFn2 \err a -> case toMaybe err of
  Nothing -> cb (Right a)
  Just err' -> cb (Left err')

handleCallback1Tuple :: forall a b. Callback1 (Tuple a b) -> JSCallback2 a b
handleCallback1Tuple cb = mkEffectFn3 \err a b -> case toMaybe err of
  Nothing -> cb (Right (Tuple a b))
  Just err' -> cb (Left err')

----------------------

datetimeToUnixEpochTimeInSeconds :: DateTime -> Int
datetimeToUnixEpochTimeInSeconds date = ms (toEpochMilliseconds date) / 1000
  where
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

