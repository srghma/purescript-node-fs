module Node.FS.Dir where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Node.FS.Dirent (Dirent, DirentNameTypeString)
import Node.Path (FilePath)

type JSCallback1 = EffectFn1 (Nullable Error) Unit
type JSCallback2 a = EffectFn2 (Nullable Error) a Unit
type Callback1 = Maybe Error -> Effect Unit
type Callback2 a = Either Error a -> Effect Unit

handleCallback2 :: forall a. Callback2 a -> JSCallback2 a
handleCallback2 cb = mkEffectFn2 \err a -> case toMaybe err of
  Nothing -> cb (Right a)
  Just err' -> cb (Left err')

-- Foreign imports for the Dir class
foreign import data Dir :: Type

foreign import closeImpl :: EffectFn2 Dir JSCallback1 Unit
foreign import closeSyncImpl :: EffectFn1 Dir Unit
foreign import readImpl :: EffectFn2 Dir (JSCallback2 (Nullable (Dirent DirentNameTypeString))) Unit
foreign import readSyncImpl :: EffectFn1 Dir (Nullable (Dirent DirentNameTypeString))

-- | Get the path of this directory as was provided to fs.opendir(), fs.opendirSync(), or fsPromises.opendir().
foreign import path :: Dir -> FilePath

-- | Asynchronously close the directory's underlying resource handle.
close :: Dir -> Callback1 -> Effect Unit
close dir callback = runEffectFn2 closeImpl dir (mkEffectFn1 $ (callback <<< toMaybe))

-- | Synchronously close the directory's underlying resource handle.
closeSync :: Dir -> Effect Unit
closeSync = runEffectFn1 closeSyncImpl

-- | Asynchronously read the next directory entry.
read :: Dir -> (Either Error (Maybe (Dirent DirentNameTypeString)) -> Effect Unit) -> Effect Unit
read dir callback = runEffectFn2 readImpl dir (handleCallback2 (callback <<< map toMaybe))

-- | Synchronously read the next directory entry.
readSync :: Dir -> Effect (Maybe (Dirent DirentNameTypeString))
readSync dir = toMaybe <$> runEffectFn1 readSyncImpl dir
