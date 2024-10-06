module Node.FS.Dir where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Node.FS.Dirent (Dirent, DirentNameTypeString)
import Node.FS.Internal.Utils (Callback0, JSCallback0, JSCallback1, Callback1, handleCallback0, handleCallback1)
import Node.Path (FilePath)

-- Foreign imports for the Dir class
foreign import data Dir :: Type

foreign import closeImpl :: EffectFn2 Dir JSCallback0 Unit
foreign import closeSyncImpl :: EffectFn1 Dir Unit
foreign import readImpl :: EffectFn2 Dir (JSCallback1 (Nullable (Dirent DirentNameTypeString))) Unit
foreign import readSyncImpl :: EffectFn1 Dir (Nullable (Dirent DirentNameTypeString))

-- | Get the path of this directory as was provided to fs.opendir(), fs.opendirSync(), or fsPromises.opendir().
foreign import path :: Dir -> FilePath

-- | Asynchronously close the directory's underlying resource handle.
close :: Dir -> Callback0 -> Effect Unit
close dir cb = runEffectFn2 closeImpl dir (handleCallback0 cb)

-- | Synchronously close the directory's underlying resource handle.
closeSync :: Dir -> Effect Unit
closeSync = runEffectFn1 closeSyncImpl

-- | Asynchronously read the next directory entry.
read :: Dir -> (Callback1 (Maybe (Dirent DirentNameTypeString))) -> Effect Unit
read dir cb = runEffectFn2 readImpl dir (handleCallback1 (cb <<< map toMaybe))

-- | Synchronously read the next directory entry.
readSync :: Dir -> Effect (Maybe (Dirent DirentNameTypeString))
readSync dir = toMaybe <$> runEffectFn1 readSyncImpl dir
