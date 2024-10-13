module Node.FS.Dir.Sync where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameString)

foreign import closeSyncImpl :: EffectFn1 Dir Unit
foreign import readSyncImpl :: EffectFn1 Dir (Nullable (Dirent DirentNameString))

-- | Synchronously close the directory's underlying resource handle.
closeSync :: Dir -> Effect Unit
closeSync = runEffectFn1 closeSyncImpl

-- | Synchronously read the next directory entry.
readSync :: Dir -> Effect (Maybe (Dirent DirentNameString))
readSync dir = toMaybe <$> runEffectFn1 readSyncImpl dir
