module Node.FS.Aff.Dir
  ( read
  , close
  , entries
  , entriesIterate
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, finally, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.FS.Dir (Dir)
import Node.FS.Dir as Dir
import Node.FS.Dirent (Dirent, DirentNameTypeString)
import Node.FS.Internal.AffUtils

read :: Dir -> Aff (Maybe (Dirent DirentNameTypeString))
read = toAff1 Dir.read

close :: Dir -> Aff (Maybe Error)
close dir = makeAff \k -> do
  Dir.close dir (k <<< Right)
  pure nonCanceler

entries :: Dir -> Aff (Array (Dirent DirentNameTypeString))
entries dir = do
  direntArrayRef <- liftEffect $ Ref.new []
  let
    handleDirent :: Dirent DirentNameTypeString -> Effect Unit
    handleDirent dirent = Ref.modify_ (flip Array.snoc dirent) direntArrayRef
  entriesIterate dir handleDirent
  liftEffect $ Ref.read direntArrayRef

-- | Implementation of `dir[Symbol.asyncIterator]()`
-- | Documentation - https://nodejs.org/docs/latest/api/fs.html#dirsymbolasynciterator
-- | Implementation - https://github.com/nodejs/node/blob/b2161d3a137e5a2582c71c798e140d2ba8f7c1d4/lib/internal/fs/dir.js#L257
-- |
-- | Nodejs version ignores errors on read, doesnt ignore errors on close.
-- | This purs version ignores error at close, doesnt ignore errors at read.
-- | But in reality - behavior should be the same (proved in tests).
-- |
-- | Possible errors:
-- | - if dir is closed already - `read` and `close` will throw "Directory handle was closed"
entriesIterate :: Dir -> ((Dirent DirentNameTypeString) -> Effect Unit) -> Aff Unit
entriesIterate dir handleDirent = finally (void $ close dir) $ makeAff \(k :: Either Error Unit -> Effect Unit) -> do
  stopRef <- Ref.new false
  go k stopRef
  pure $ effectCanceler $ Ref.write true stopRef
  where
  go :: (Either Error Unit -> Effect Unit) -> Ref Boolean -> Effect Unit
  go callback stopRef = do
    stopped <- Ref.read stopRef
    if stopped then do
      callback $ Right unit
    else Dir.read dir case _ of
      Left error -> callback $ Left error
      Right maybeDirent ->
        case maybeDirent of
          Nothing -> do
            callback $ Right unit
          Just dirent -> do
            handleDirent dirent
            go callback stopRef
