module Node.FS.Async
  ( Callback
  , access
  , access'
  , copyFile
  , copyFile'
  , mkdtemp
  , mkdtemp'
  , rename
  , truncate
  , chown
  , chmod
  , lstat
  , stat
  , link
  , symlink
  , readlink
  , realpath
  , realpath'
  , unlink
  , rmdir
  , rmdir'
  , rm
  , rm'
  , mkdir
  , mkdir'
  , readdir
  , readdir'
  , readdirBuffer
  , readdirBuffer'
  , readdirDirent
  , readdirDirent'
  , readdirDirentBuffer
  , readdirDirentBuffer'
  , utimes
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
  , appendFile
  , appendTextFile
  , fdOpen
  , fdRead
  , fdNext
  , fdWrite
  , fdAppend
  , fdClose
  , cp
  , cp'
  , cpOptionsDefault
  , CpOptions
  , CpForce(..)
  , fchmod
  , fchown
  , fdatasync
  , fstat
  , fsync
  , ftruncate
  , futimes
  , glob
  , glob'
  , globDirent
  , globDirent'
  , lchmod
  , lchown
  , lutimes
  -- , openAsBlob
  , opendir'
  , opendir
  , readv
  , statfs
  -- , unwatchFile
  -- , watch
  -- , watchFile
  , writev
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn6, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn6)
import Node.Buffer (Buffer, size)
import Node.Encoding (Encoding(..), encodingToNode)
import Node.FS (FileDescriptor, ByteCount, FilePosition, BufferLength, BufferOffset, FileMode, SymlinkType, symlinkTypeToNode)
import Node.FS.Constants (FileFlags, fileFlagsToNode, AccessMode, CopyMode, defaultAccessMode, defaultCopyMode)
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameTypeBuffer, DirentNameTypeString)
import Node.FS.Perms (Perms, permsToString, all, mkPerms)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)

datetimeToUnixEpochTimeInSeconds :: DateTime -> Int
datetimeToUnixEpochTimeInSeconds date = ms (toEpochMilliseconds date) / 1000
  where
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

type JSCallback a = EffectFn2 (Nullable Error) a Unit

type JSCallback2 a b = EffectFn3 (Nullable Error) a b Unit

handleCallback :: forall a. Callback a -> JSCallback a
handleCallback cb = mkEffectFn2 \err a -> case toMaybe err of
  Nothing -> cb (Right a)
  Just err' -> cb (Left err')

handleCallback2 :: forall a b. Callback (Tuple a b) -> JSCallback2 a b
handleCallback2 cb = mkEffectFn3 \err a b -> case toMaybe err of
  Nothing -> cb (Right (Tuple a b))
  Just err' -> cb (Left err')

-- | Type synonym for callback functions.
type Callback a = Either Error a -> Effect Unit

access :: FilePath -> (Maybe Error -> Effect Unit) -> Effect Unit
access path = access' path defaultAccessMode

access' :: FilePath -> AccessMode -> (Maybe Error -> Effect Unit) -> Effect Unit
access' path mode cb = runEffectFn3 accessImpl path mode $ mkEffectFn1 \err -> do
  cb $ toMaybe err

foreign import accessImpl :: EffectFn3 FilePath AccessMode (EffectFn1 (Nullable Error) Unit) Unit

copyFile :: FilePath -> FilePath -> Callback Unit -> Effect Unit
copyFile src dest = copyFile' src dest defaultCopyMode

copyFile' :: FilePath -> FilePath -> CopyMode -> Callback Unit -> Effect Unit
copyFile' src dest mode cb = runEffectFn4 copyFileImpl src dest mode (handleCallback cb)

foreign import copyFileImpl :: EffectFn4 FilePath FilePath CopyMode (JSCallback Unit) Unit

mkdtemp :: FilePath -> Callback FilePath -> Effect Unit
mkdtemp prefix = mkdtemp' prefix UTF8

mkdtemp' :: FilePath -> Encoding -> Callback FilePath -> Effect Unit
mkdtemp' prefix encoding cb = runEffectFn3 mkdtempImpl prefix (encodingToNode encoding) (handleCallback cb)

foreign import mkdtempImpl :: EffectFn3 FilePath FilePath (JSCallback FilePath) Unit

foreign import renameImpl :: EffectFn3 FilePath FilePath (JSCallback Unit) Unit
foreign import truncateImpl :: EffectFn3 FilePath Int (JSCallback Unit) Unit
foreign import chownImpl :: EffectFn4 FilePath Int Int (JSCallback Unit) Unit
foreign import chmodImpl :: EffectFn3 FilePath String (JSCallback Unit) Unit
foreign import statImpl :: EffectFn2 FilePath (JSCallback Stats) Unit
foreign import lstatImpl :: EffectFn2 FilePath (JSCallback Stats) Unit
foreign import linkImpl :: EffectFn3 FilePath FilePath (JSCallback Unit) Unit
foreign import symlinkImpl :: EffectFn4 FilePath FilePath (Nullable String) (JSCallback Unit) Unit
foreign import readlinkImpl :: EffectFn2 FilePath (JSCallback FilePath) Unit
foreign import realpathImpl :: forall cache. EffectFn3 FilePath { | cache } (JSCallback FilePath) Unit
foreign import unlinkImpl :: EffectFn2 FilePath (JSCallback Unit) Unit
foreign import rmdirImpl :: EffectFn3 FilePath { maxRetries :: Int, retryDelay :: Int } (JSCallback Unit) Unit
foreign import rmImpl :: EffectFn3 FilePath { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int } (JSCallback Unit) Unit
foreign import mkdirImpl :: EffectFn3 FilePath { recursive :: Boolean, mode :: String } (JSCallback Unit) Unit
-- if { withFileTypes: false, recursive: false } => ['Tidy']
-- if { withFileTypes: false, recursive: true } => [ 'Tidy', 'Tidy/Codegen', 'Tidy/Codegen.purs', 'Tidy/Codegen/Class.purs', .. ]
foreign import readdirImpl :: forall filepathOrDirentOrBuffer stringOrBuffer. EffectFn3 FilePath { encoding :: stringOrBuffer, recursive :: Boolean, withFileTypes :: Boolean } (JSCallback (Array filepathOrDirentOrBuffer)) Unit
foreign import utimesImpl :: EffectFn4 FilePath Int Int (JSCallback Unit) Unit
foreign import readFileImpl :: forall a opts. EffectFn3 FilePath { | opts } (JSCallback a) Unit
foreign import writeFileImpl :: forall a opts. EffectFn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import appendFileImpl :: forall a opts. EffectFn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import openImpl :: EffectFn4 FilePath String (Nullable FileMode) (JSCallback FileDescriptor) Unit
foreign import readImpl :: EffectFn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import writeImpl :: EffectFn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import closeImpl :: EffectFn2 FileDescriptor (JSCallback Unit) Unit
foreign import cpImpl :: EffectFn4 FilePath FilePath CpOptionsInternal (JSCallback Unit) Unit
foreign import fchmodImpl :: EffectFn3 FileDescriptor String (JSCallback Unit) Unit
foreign import fchownImpl :: EffectFn4 FileDescriptor Int Int (JSCallback Unit) Unit
foreign import fdatasyncImpl :: EffectFn2 FileDescriptor (JSCallback Unit) Unit
foreign import fstatImpl :: EffectFn2 FileDescriptor (JSCallback Stats) Unit
foreign import fsyncImpl :: EffectFn2 FileDescriptor (JSCallback Unit) Unit
foreign import ftruncateImpl :: EffectFn3 FileDescriptor Int (JSCallback Unit) Unit
foreign import futimesImpl :: EffectFn4 FileDescriptor Int Int (JSCallback Unit) Unit
foreign import globImpl :: forall filepathOrDirent. EffectFn3 (Array FilePath) { cwd :: Nullable FilePath, exclude :: Nullable (filepathOrDirent -> Boolean), withFileTypes :: Boolean } (JSCallback (Array filepathOrDirent)) Unit
foreign import lchmodImpl :: EffectFn3 FilePath String (JSCallback Unit) Unit
foreign import lchownImpl :: EffectFn4 FilePath Int Int (JSCallback Unit) Unit
foreign import lutimesImpl :: EffectFn4 FilePath Int Int (JSCallback Unit) Unit
-- foreign import openAsBlobImpl :: EffectFn2 FilePath (Promise Blob) Unit
foreign import opendirImpl :: EffectFn3 FilePath { bufferSize :: Int, recursive :: Boolean, encoding :: String } (JSCallback Dir) Unit
foreign import readvImpl :: EffectFn4 FileDescriptor (Array Buffer) (Nullable FilePosition) (JSCallback2 ByteCount (Array Buffer)) Unit
foreign import statfsImpl :: EffectFn2 FilePath (JSCallback Stats) Unit
-- foreign import unwatchFileImpl :: EffectFn1 FilePath Unit
-- foreign import watchImpl :: EffectFn2 FilePath (EffectFn1 String Unit) Unit
-- foreign import watchFileImpl :: EffectFn2 FilePath (EffectFn2 Stats Stats Unit) Unit
foreign import writevImpl :: EffectFn4 FileDescriptor (Array Buffer) (Nullable FilePosition) (JSCallback2 ByteCount (Array Buffer)) Unit

-- | Renames a file.
rename
  :: FilePath
  -> FilePath
  -> Callback Unit
  -> Effect Unit
rename oldFile newFile cb = runEffectFn3 renameImpl oldFile newFile (handleCallback cb)

-- | Truncates a file to the specified length.
truncate
  :: FilePath
  -> Int
  -> Callback Unit
  -> Effect Unit
truncate file len cb = runEffectFn3 truncateImpl file len (handleCallback cb)

-- | Changes the ownership of a file.
chown
  :: FilePath
  -> Int
  -> Int
  -> Callback Unit
  -> Effect Unit
chown file uid gid cb = runEffectFn4 chownImpl file uid gid (handleCallback cb)

-- | Changes the permissions of a file.
chmod
  :: FilePath
  -> Perms
  -> Callback Unit
  -> Effect Unit
chmod file perms cb = runEffectFn3 chmodImpl file (permsToString perms) (handleCallback cb)

-- | Gets file statistics.
stat
  :: FilePath
  -> Callback Stats
  -> Effect Unit
stat file cb = runEffectFn2 statImpl file (handleCallback $ cb)

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat
  :: FilePath
  -> Callback Stats
  -> Effect Unit
lstat file cb = runEffectFn2 lstatImpl file (handleCallback $ cb)

-- | Creates a link to an existing file.
link
  :: FilePath
  -> FilePath
  -> Callback Unit
  -> Effect Unit
link src dst cb = runEffectFn3 linkImpl src dst (handleCallback cb)

-- | Creates a symlink.
symlink
  :: FilePath
  -> FilePath
  -> SymlinkType
  -> Callback Unit
  -> Effect Unit
symlink src dest ty cb = runEffectFn4 symlinkImpl src dest (symlinkTypeToNode ty) (handleCallback cb)

-- | Reads the value of a symlink.
readlink
  :: FilePath
  -> Callback FilePath
  -> Effect Unit
readlink path cb = runEffectFn2 readlinkImpl path (handleCallback cb)

-- | Find the canonicalized absolute location for a path.
realpath
  :: FilePath
  -> Callback FilePath
  -> Effect Unit
realpath path cb = runEffectFn3 realpathImpl path {} (handleCallback cb)

-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
realpath'
  :: forall cache
   . FilePath
  -> { | cache }
  -> Callback FilePath
  -> Effect Unit
realpath' path cache cb = runEffectFn3 realpathImpl path cache (handleCallback cb)

-- | Deletes a file.
unlink
  :: FilePath
  -> Callback Unit
  -> Effect Unit
unlink file cb = runEffectFn2 unlinkImpl file (handleCallback cb)

-- | Deletes a directory.
rmdir
  :: FilePath
  -> Callback Unit
  -> Effect Unit
rmdir path cb = rmdir' path { maxRetries: 0, retryDelay: 100 } cb

-- | Deletes a directory with options.
rmdir'
  :: FilePath
  -> { maxRetries :: Int, retryDelay :: Int }
  -> Callback Unit
  -> Effect Unit
rmdir' path opts cb = runEffectFn3 rmdirImpl path opts (handleCallback cb)

-- | Deletes a file or directory.
rm
  :: FilePath
  -> Callback Unit
  -> Effect Unit
rm path = rm' path { force: false, maxRetries: 100, recursive: false, retryDelay: 1000 }

-- | Deletes a file or directory with options.
rm'
  :: FilePath
  -> { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int }
  -> Callback Unit
  -> Effect Unit
rm' path opts cb = runEffectFn3 rmImpl path opts (handleCallback cb)

-- | Makes a new directory.
mkdir
  :: FilePath
  -> Callback Unit
  -> Effect Unit
mkdir path = mkdir' path { recursive: false, mode: mkPerms all all all }

-- | Makes a new directory with the specified permissions.
mkdir'
  :: FilePath
  -> { recursive :: Boolean, mode :: Perms }
  -> Callback Unit
  -> Effect Unit
mkdir' file { recursive, mode: perms } cb = runEffectFn3 mkdirImpl file { recursive, mode: permsToString perms } (handleCallback cb)

-- | Reads the contents of a directory.
readdir
  :: FilePath
  -> Callback (Array FilePath)
  -> Effect Unit
readdir file = readdir' file { recursive: false, encoding: UTF8 }

-- | Reads the contents of a directory.
readdir'
  :: FilePath
  -> { recursive :: Boolean, encoding :: Encoding }
  -> Callback (Array FilePath)
  -> Effect Unit
readdir' file { recursive, encoding } cb = runEffectFn3 readdirImpl file { recursive, encoding: encodingToNode encoding, withFileTypes: false } (handleCallback cb)

-- | Reads the contents of a directory.
readdirBuffer
  :: FilePath
  -> Callback (Array Buffer)
  -> Effect Unit
readdirBuffer file = readdirBuffer' file { recursive: false }

-- | Reads the contents of a directory.
readdirBuffer'
  :: FilePath
  -> { recursive :: Boolean }
  -> Callback (Array Buffer)
  -> Effect Unit
readdirBuffer' file { recursive } cb = runEffectFn3 readdirImpl file { recursive, encoding: "buffer", withFileTypes: false } (handleCallback cb)

-- | Reads the contents of a directory.
readdirDirent
  :: FilePath
  -> Callback (Array (Dirent DirentNameTypeString))
  -> Effect Unit
readdirDirent file = readdirDirent' file { recursive: false, encoding: UTF8 }

-- | Reads the contents of a directory.
readdirDirent'
  :: FilePath
  -> { recursive :: Boolean, encoding :: Encoding }
  -> Callback (Array (Dirent DirentNameTypeString))
  -> Effect Unit
readdirDirent' file { recursive, encoding } cb = runEffectFn3 readdirImpl file { recursive, encoding: encodingToNode encoding, withFileTypes: true } (handleCallback cb)

-- | Reads the contents of a directory.
readdirDirentBuffer
  :: FilePath
  -> Callback (Array (Dirent DirentNameTypeBuffer))
  -> Effect Unit
readdirDirentBuffer file = readdirDirentBuffer' file { recursive: false }

-- | Reads the contents of a directory.
readdirDirentBuffer'
  :: FilePath
  -> { recursive :: Boolean }
  -> Callback (Array (Dirent DirentNameTypeBuffer))
  -> Effect Unit
readdirDirentBuffer' file { recursive } cb = runEffectFn3 readdirImpl file { recursive, encoding: "buffer", withFileTypes: true } (handleCallback cb)

-- | Sets the accessed and modified times for the specified file.
utimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Callback Unit
  -> Effect Unit
utimes file atime mtime cb = runEffectFn4 utimesImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime) (handleCallback cb)

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile
  :: FilePath
  -> Callback Buffer
  -> Effect Unit
readFile file cb = runEffectFn3 readFileImpl file {} (handleCallback cb)

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile
  :: Encoding
  -> FilePath
  -> Callback String
  -> Effect Unit
readTextFile encoding file cb = runEffectFn3 readFileImpl file { encoding: show encoding } (handleCallback cb)

-- | Writes a buffer to a file.
writeFile
  :: FilePath
  -> Buffer
  -> Callback Unit
  -> Effect Unit
writeFile file buff cb = runEffectFn4 writeFileImpl file buff {} (handleCallback cb)

-- | Writes text to a file using the specified encoding.
writeTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Callback Unit
  -> Effect Unit
writeTextFile encoding file buff cb = runEffectFn4 writeFileImpl file buff { encoding: show encoding } (handleCallback cb)

-- | Appends the contents of a buffer to a file.
appendFile
  :: FilePath
  -> Buffer
  -> Callback Unit
  -> Effect Unit
appendFile file buff cb = runEffectFn4 appendFileImpl file buff {} (handleCallback cb)

-- | Appends text to a file using the specified encoding.
appendTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Callback Unit
  -> Effect Unit
appendTextFile encoding file buff cb = runEffectFn4 appendFileImpl file buff { encoding: show encoding } (handleCallback cb)

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen
  :: FilePath
  -> FileFlags
  -> Maybe FileMode
  -> Callback FileDescriptor
  -> Effect Unit
fdOpen file flags mode cb = runEffectFn4 openImpl file (fileFlagsToNode flags) (toNullable mode) (handleCallback cb)

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Callback ByteCount
  -> Effect Unit
fdRead fd buff off len pos cb = runEffectFn6 readImpl fd buff off len (toNullable pos) (handleCallback cb)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext
  :: FileDescriptor
  -> Buffer
  -> Callback ByteCount
  -> Effect Unit
fdNext fd buff cb = do
  sz <- size buff
  fdRead fd buff 0 sz Nothing cb

-- | Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
-- | for details.
fdWrite
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Callback ByteCount
  -> Effect Unit
fdWrite fd buff off len pos cb = runEffectFn6 writeImpl fd buff off len (toNullable pos) (handleCallback cb)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend
  :: FileDescriptor
  -> Buffer
  -> Callback ByteCount
  -> Effect Unit
fdAppend fd buff cb = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing cb

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose
  :: FileDescriptor
  -> Callback Unit
  -> Effect Unit
fdClose fd cb = runEffectFn2 closeImpl fd (handleCallback cb)

data CpForce = CpForce_False | CpForce_TrueWithoutErrorOnExit | CpForce_TrueWithErrorOnExit

type CpOptionsInternal =
  { dereference :: Boolean
  , errorOnExist :: Boolean
  , filter :: Nullable (Fn2 FilePath FilePath Boolean)
  , force :: Boolean
  , mode :: FileMode
  , preserveTimestamps :: Boolean
  , recursive :: Boolean
  , verbatimSymlinks :: Boolean
  }

type CpOptions =
  { dereference :: Boolean -- Whether to dereference symlinks
  , filter :: Maybe (FilePath -> FilePath -> Boolean)
  , force :: CpForce
  , mode :: FileMode -- Modifiers for copy operation
  , preserveTimestamps :: Boolean -- Preserve timestamps from source
  , recursive :: Boolean -- Copy directories recursively
  , verbatimSymlinks :: Boolean -- Skip path resolution for symlinks
  }

cpOptionsToCpOptionsInternal :: CpOptions -> CpOptionsInternal
cpOptionsToCpOptionsInternal opts =
  { dereference: opts.dereference
  , errorOnExist: case opts.force of
      CpForce_TrueWithErrorOnExit -> true
      _ -> false
  , filter: toNullable $ map mkFn2 (opts.filter)
  , force: case opts.force of
      CpForce_False -> false
      _ -> true
  , mode: opts.mode
  , preserveTimestamps: opts.preserveTimestamps
  , recursive: opts.recursive
  , verbatimSymlinks: opts.verbatimSymlinks
  }

cpOptionsDefault :: CpOptions
cpOptionsDefault =
  { dereference: false
  , filter: Nothing
  , force: CpForce_TrueWithoutErrorOnExit
  , mode: 0
  , preserveTimestamps: false
  , recursive: false
  , verbatimSymlinks: false
  }

-- | Copy a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fspromises_copyfile_src_dest_mode)
-- | for details.
cp :: FilePath -> FilePath -> Callback Unit -> Effect Unit
cp src dest = cp' src dest cpOptionsDefault

cp' :: FilePath -> FilePath -> CpOptions -> Callback Unit -> Effect Unit
cp' src dest opts cb = runEffectFn4 cpImpl src dest (cpOptionsToCpOptionsInternal opts) (handleCallback cb)

-- | Change permissions on a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchmod_fd_mode_callback)
-- | for details.
fchmod :: FileDescriptor -> Perms -> Callback Unit -> Effect Unit
fchmod fd perms cb = runEffectFn3 fchmodImpl fd (permsToString perms) (handleCallback cb)

-- | Change ownership of a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchown_fd_uid_gid_callback)
-- | for details.
fchown :: FileDescriptor -> Int -> Int -> Callback Unit -> Effect Unit
fchown fd uid gid cb = runEffectFn4 fchownImpl fd uid gid (handleCallback cb)

-- | Synchronize a file's in-core state with storage. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fdatasync_fd_callback)
-- | for details.
fdatasync :: FileDescriptor -> Callback Unit -> Effect Unit
fdatasync fd cb = runEffectFn2 fdatasyncImpl fd (handleCallback cb)

-- | Get file status information. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fstat_fd_callback)
-- | for details.
fstat :: FileDescriptor -> Callback Stats -> Effect Unit
fstat fd cb = runEffectFn2 fstatImpl fd (handleCallback cb)

-- | Flushes a file descriptor to disk. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fsync_fd_callback)
-- | for details.
fsync :: FileDescriptor -> Callback Unit -> Effect Unit
fsync fd cb = runEffectFn2 fsyncImpl fd (handleCallback cb)

-- | Truncate a file to a specified length. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_ftruncate_fd_len_callback)
-- | for details.
ftruncate :: FileDescriptor -> Int -> Callback Unit -> Effect Unit
ftruncate fd len cb = runEffectFn3 ftruncateImpl fd len (handleCallback cb)

-- | Change file timestamps for a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_futimes_fd_atime_mtime_callback)
-- | for details.
futimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Callback Unit
  -> Effect Unit
futimes file atime mtime cb = runEffectFn4 lutimesImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime) (handleCallback cb)

-- | Perform pattern matching in file paths. See the [Node Documentation](https://nodejs.org/api/glob.html#globglob_pattern_options_callback)
-- | for details.
glob :: Array FilePath -> Callback (Array FilePath) -> Effect Unit
glob pattern = glob' pattern { cwd: Nothing, exclude: Nothing }

glob' :: Array FilePath -> { cwd :: Maybe FilePath, exclude :: Maybe (FilePath -> Boolean) } -> Callback (Array FilePath) -> Effect Unit
glob' pattern { cwd, exclude } cb = runEffectFn3 globImpl pattern { cwd: toNullable cwd, exclude: toNullable exclude, withFileTypes: false } (handleCallback cb)

globDirent :: Array FilePath -> Callback (Array (Dirent DirentNameTypeString)) -> Effect Unit
globDirent pattern = globDirent' pattern { cwd: Nothing, exclude: Nothing }

globDirent' :: Array FilePath -> { cwd :: Maybe FilePath, exclude :: Maybe (Dirent DirentNameTypeString -> Boolean) } -> Callback (Array (Dirent DirentNameTypeString)) -> Effect Unit
globDirent' pattern { cwd, exclude } cb = runEffectFn3 globImpl pattern { cwd: toNullable cwd, exclude: toNullable exclude, withFileTypes: true } (handleCallback cb)

-- | Change permissions on a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchmod_path_mode_callback)
-- | for details.
lchmod :: FilePath -> Perms -> Callback Unit -> Effect Unit
lchmod path perms cb = runEffectFn3 lchmodImpl path (permsToString perms) (handleCallback cb)

-- | Change ownership of a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchown_path_uid_gid_callback)
-- | for details.
lchown :: FilePath -> Int -> Int -> Callback Unit -> Effect Unit
lchown path uid gid cb = runEffectFn4 lchownImpl path uid gid (handleCallback cb)

-- | Change timestamps for a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lutimes_path_atime_mtime_callback)
-- | for details.
lutimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Callback Unit
  -> Effect Unit
lutimes file atime mtime cb = runEffectFn4 lutimesImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime) (handleCallback cb)

-- | TODO: path - Buffer Url, returns Promise
-- | Open a file as a blob. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_class_filehandle)
-- | for details.
-- openAsBlob :: FilePath -> Promise Blob -> Effect Unit
-- openAsBlob path cb = runEffectFn2 openAsBlobImpl path (handleCallback cb)

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
opendir' :: FilePath -> { encoding :: Encoding, bufferSize :: Int, recursive :: Boolean } -> Callback Dir -> Effect Unit
opendir' path { encoding, bufferSize, recursive } cb = runEffectFn3 opendirImpl path { encoding: encodingToNode encoding, bufferSize, recursive } (handleCallback cb)

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
-- | NOTE: encoding: 'buffer' is not supported, will throw error "TypeError [ERR_INVALID_ARG_TYPE]: The "path" argument must be of type string. Received an instance of Buffer"
opendir :: FilePath -> Callback Dir -> Effect Unit
opendir path = opendir' path { bufferSize: 32, recursive: false, encoding: UTF8 }

-- | Read from a file descriptor into a buffer array. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_readv_fd_buffers_position_callback)
-- | for details.
readv :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Callback (Tuple ByteCount (Array Buffer)) -> Effect Unit
readv fd buffers position cb = runEffectFn4 readvImpl fd buffers (toNullable position) (handleCallback2 cb)

-- | Get file system statistics. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_statfs_path_callback)
-- | for details.
statfs :: FilePath -> Callback Stats -> Effect Unit
statfs path cb = runEffectFn2 statfsImpl path (handleCallback cb)

-- -- | Stop watching a file for changes. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_unwatchfile_filename_listener)
-- -- | for details.
-- unwatchFile :: FilePath -> Effect Unit
-- unwatchFile path = runEffectFn1 unwatchFileImpl path
--
-- -- | Watch for changes in a file or directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_watch_filename_options_listener)
-- -- | for details.
-- watch :: FilePath -> (String -> Effect Unit) -> Effect Unit
-- watch path cb = runEffectFn2 watchImpl path (mkEffectFn1 cb)
--
-- -- | Watch for changes in a file and trigger a callback. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_watchfile_filename_options_listener)
-- -- | for details.
-- watchFile :: FilePath -> (Stats -> Stats -> Effect Unit) -> Effect Unit
-- watchFile path cb = runEffectFn2 watchFileImpl path (mkEffectFn2 cb)

-- | Write from an array of buffers to a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_writev_fd_buffers_position_callback)
-- | for details.
writev :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Callback (Tuple ByteCount (Array Buffer)) -> Effect Unit
writev fd buffers position cb = runEffectFn4 writevImpl fd buffers (toNullable position) (handleCallback2 cb)
