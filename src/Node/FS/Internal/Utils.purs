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

datetimeToUnixEpochTimeInSeconds :: DateTime -> Int
datetimeToUnixEpochTimeInSeconds date = ms (toEpochMilliseconds date) / 1000
  where
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

