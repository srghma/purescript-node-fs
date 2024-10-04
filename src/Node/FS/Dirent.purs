module Node.FS.Dirent where

import Prelude

import Node.Buffer (Buffer)
import Node.Path (FilePath)
import Unsafe.Coerce (unsafeCoerce)

data DirentNameType

foreign import data DirentNameTypeString :: DirentNameType
foreign import data DirentNameTypeBuffer :: DirentNameType

foreign import data Dirent :: DirentNameType -> Type

-- | Check if the Dirent object describes a block device.
foreign import isBlockDevice :: forall direntnametype. Dirent direntnametype -> Boolean

-- | Check if the Dirent object describes a character device.
foreign import isCharacterDevice :: forall direntnametype. Dirent direntnametype -> Boolean

-- | Check if the Dirent object describes a directory.
foreign import isDirectory :: forall direntnametype. Dirent direntnametype -> Boolean

-- | Check if the Dirent object describes a FIFO pipe.
foreign import isFIFO :: forall direntnametype. Dirent direntnametype -> Boolean

-- | Check if the Dirent object describes a regular file.
foreign import isFile :: forall direntnametype. Dirent direntnametype -> Boolean

-- | Check if the Dirent object describes a socket.
foreign import isSocket :: forall direntnametype. Dirent direntnametype -> Boolean

-- | Check if the Dirent object describes a symbolic link.
foreign import isSymbolicLink :: forall direntnametype. Dirent direntnametype -> Boolean

foreign import nameImpl :: forall direntnametype nametype. Dirent direntnametype -> nametype

class IsDirentNameForDirent direntnametype stringOrBuffer | direntnametype -> stringOrBuffer where
  -- | The file name that this <fs.Dirent> object refers to. The type of this value is determined by the options.encoding passed to fs.readdir() or fs.readdirSync().
  name :: Dirent direntnametype -> stringOrBuffer

instance IsDirentNameForDirent DirentNameTypeString String where
  name :: Dirent DirentNameTypeString -> String
  name = unsafeCoerce nameImpl
else instance IsDirentNameForDirent DirentNameTypeBuffer Buffer where
  name :: Dirent DirentNameTypeBuffer -> Buffer
  name = unsafeCoerce nameImpl

-- | Get the parent directory path of the file this Dirent object refers to.
-- | Added in: v21.4.0, v20.12.0, v18.20.0
-- | TODO: support for old node version? `Nullable FilePath`?
foreign import parentPath :: forall direntnametype. Dirent direntnametype -> FilePath

foreign import showDirentObj :: forall direntnametype. Dirent direntnametype -> String

instance Show (Dirent direntnametype) where
  show s = "Dirent " <> showDirentObj s
