module Node.FS.Async
  ( access
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
  , fdRead'
  , fdNext
  , fdWrite
  , fdWrite'
  , fdWriteString
  , fdAppend
  , fdClose
  , cp
  , cp'
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
  , opendir
  , opendir'
  , readv
  , statfs
  -- , unwatchFile
  -- , watch
  -- , watchFile
  , writev
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, mkEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn6)
import Node.Buffer (Buffer, size)
import Node.Encoding (Encoding(..), encodingToNode)
import Node.FS (FileDescriptor, ByteCount, FilePosition, BufferLength, BufferOffset, FileMode, SymlinkType, symlinkTypeToNode)
import Node.FS.Constants
import Node.FS.Internal.Utils 
import Node.FS.Options
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameTypeBuffer, DirentNameTypeString)
import Node.FS.Perms (Perms, permsToString)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)
import Unsafe.Coerce (unsafeCoerce)

foreign import accessImpl :: EffectFn3 FilePath AccessMode (EffectFn1 (Nullable Error) Unit) Unit
foreign import copyFileImpl :: EffectFn4 FilePath FilePath CopyMode (JSCallback1 Unit) Unit
foreign import mkdtempImpl :: EffectFn3 FilePath FilePath (JSCallback1 FilePath) Unit
foreign import renameImpl :: EffectFn3 FilePath FilePath (JSCallback1 Unit) Unit
foreign import truncateImpl :: EffectFn3 FilePath Int (JSCallback1 Unit) Unit
foreign import chownImpl :: EffectFn4 FilePath Int Int (JSCallback1 Unit) Unit
foreign import chmodImpl :: EffectFn3 FilePath String (JSCallback1 Unit) Unit
foreign import statImpl :: EffectFn2 FilePath (JSCallback1 Stats) Unit
foreign import lstatImpl :: EffectFn2 FilePath (JSCallback1 Stats) Unit
foreign import linkImpl :: EffectFn3 FilePath FilePath (JSCallback1 Unit) Unit
foreign import symlinkImpl :: EffectFn4 FilePath FilePath (Nullable String) (JSCallback1 Unit) Unit
foreign import readlinkImpl :: EffectFn2 FilePath (JSCallback1 FilePath) Unit
foreign import realpathImpl :: EffectFn3 FilePath RealpathOptionsInternal (JSCallback1 FilePath) Unit
foreign import unlinkImpl :: EffectFn2 FilePath (JSCallback1 Unit) Unit
foreign import rmdirImpl :: EffectFn3 FilePath RmdirOptions (JSCallback1 Unit) Unit
foreign import rmImpl :: EffectFn3 FilePath RmOptions (JSCallback1 Unit) Unit
foreign import mkdirImpl :: EffectFn3 FilePath MkdirOptionsInternal (JSCallback1 Unit) Unit
-- if { withFileTypes: false, recursive: false } => ['Tidy']
-- if { withFileTypes: false, recursive: true } => [ 'Tidy', 'Tidy/Codegen', 'Tidy/Codegen.purs', 'Tidy/Codegen/Class.purs', .. ]
foreign import readdirImpl :: forall filepathOrDirentOrBuffer . EffectFn3 FilePath ReaddirOptionsInternal (JSCallback1 (Array filepathOrDirentOrBuffer)) Unit
foreign import utimesImpl :: EffectFn4 FilePath Int Int (JSCallback1 Unit) Unit
foreign import readFileImpl :: forall stringOrBuffer . EffectFn3 FilePath ReadFileOptionsInternal (JSCallback1 stringOrBuffer) Unit
foreign import writeFileImpl :: forall stringOrBuffer . EffectFn4 FilePath stringOrBuffer WriteFileOptionsInternal (JSCallback1 Unit) Unit
foreign import appendFileImpl :: forall stringOrBuffer . EffectFn4 FilePath stringOrBuffer AppendFileOptionsInternal (JSCallback1 Unit) Unit
foreign import openImpl :: EffectFn4 FilePath String (Nullable FileMode) (JSCallback1 FileDescriptor) Unit
foreign import readImpl :: EffectFn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback1 ByteCount) Unit

-- https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback
readWithOptionsImpl :: EffectFn3 FileDescriptor FdReadOptionsInternal (JSCallback2 ByteCount Buffer) Unit
readWithOptionsImpl = unsafeCoerce readImpl

foreign import writeImpl :: EffectFn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback1 ByteCount) Unit

-- https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback
writeWithOptionsImpl :: EffectFn3 FileDescriptor FdWriteOptionsInternal (JSCallback2 ByteCount Buffer) Unit
writeWithOptionsImpl = unsafeCoerce writeImpl

-- https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback
writeStringImpl :: EffectFn5 FileDescriptor String (Nullable FilePosition) String (JSCallback2 ByteCount String) Unit
writeStringImpl = unsafeCoerce writeImpl

foreign import closeImpl :: EffectFn2 FileDescriptor (JSCallback1 Unit) Unit
foreign import cpImpl :: EffectFn4 FilePath FilePath CpOptionsInternal (JSCallback1 Unit) Unit
foreign import fchmodImpl :: EffectFn3 FileDescriptor String (JSCallback1 Unit) Unit
foreign import fchownImpl :: EffectFn4 FileDescriptor Int Int (JSCallback1 Unit) Unit
foreign import fdatasyncImpl :: EffectFn2 FileDescriptor (JSCallback1 Unit) Unit
foreign import fstatImpl :: EffectFn2 FileDescriptor (JSCallback1 Stats) Unit
foreign import fsyncImpl :: EffectFn2 FileDescriptor (JSCallback1 Unit) Unit
foreign import ftruncateImpl :: EffectFn3 FileDescriptor Int (JSCallback1 Unit) Unit
foreign import futimesImpl :: EffectFn4 FileDescriptor Int Int (JSCallback1 Unit) Unit
foreign import globImpl :: forall filepathOrDirent. EffectFn3 (Array FilePath) (GlobOptionsInternal filepathOrDirent) (JSCallback1 (Array filepathOrDirent)) Unit
foreign import lchmodImpl :: EffectFn3 FilePath String (JSCallback1 Unit) Unit
foreign import lchownImpl :: EffectFn4 FilePath Int Int (JSCallback1 Unit) Unit
foreign import lutimesImpl :: EffectFn4 FilePath Int Int (JSCallback1 Unit) Unit
-- foreign import openAsBlobImpl :: EffectFn2 FilePath (Promise Blob) Unit
foreign import opendirImpl :: EffectFn3 FilePath OpendirOptionsInternal (JSCallback1 Dir) Unit
foreign import readvImpl :: EffectFn4 FileDescriptor (Array Buffer) (Nullable FilePosition) (JSCallback2 ByteCount (Array Buffer)) Unit
foreign import statfsImpl :: EffectFn2 FilePath (JSCallback1 Stats) Unit
-- foreign import unwatchFileImpl :: EffectFn1 FilePath Unit
-- foreign import watchImpl :: EffectFn2 FilePath (EffectFn1 String Unit) Unit
-- foreign import watchFileImpl :: EffectFn2 FilePath (EffectFn2 Stats Stats Unit) Unit
foreign import writevImpl :: EffectFn4 FileDescriptor (Array Buffer) (Nullable FilePosition) (JSCallback2 ByteCount (Array Buffer)) Unit

access :: FilePath -> (Maybe Error -> Effect Unit) -> Effect Unit
access path = access' path defaultAccessMode

access' :: FilePath -> AccessMode -> (Maybe Error -> Effect Unit) -> Effect Unit
access' path mode cb = runEffectFn3 accessImpl path mode $ mkEffectFn1 (cb <<< toMaybe)

copyFile :: FilePath -> FilePath -> Callback1 Unit -> Effect Unit
copyFile src dest = copyFile' src dest defaultCopyMode

copyFile' :: FilePath -> FilePath -> CopyMode -> Callback1 Unit -> Effect Unit
copyFile' src dest mode cb = runEffectFn4 copyFileImpl src dest mode (handleCallback1 cb)

mkdtemp :: FilePath -> Callback1 FilePath -> Effect Unit
mkdtemp prefix = mkdtemp' prefix UTF8

mkdtemp' :: FilePath -> Encoding -> Callback1 FilePath -> Effect Unit
mkdtemp' prefix encoding cb = runEffectFn3 mkdtempImpl prefix (encodingToNode encoding) (handleCallback1 cb)

-- | Renames a file.
rename
  :: FilePath
  -> FilePath
  -> Callback1 Unit
  -> Effect Unit
rename oldFile newFile cb = runEffectFn3 renameImpl oldFile newFile (handleCallback1 cb)

-- | Truncates a file to the specified length.
truncate
  :: FilePath
  -> Int
  -> Callback1 Unit
  -> Effect Unit
truncate file len cb = runEffectFn3 truncateImpl file len (handleCallback1 cb)

-- | Changes the ownership of a file.
chown
  :: FilePath
  -> Int
  -> Int
  -> Callback1 Unit
  -> Effect Unit
chown file uid gid cb = runEffectFn4 chownImpl file uid gid (handleCallback1 cb)

-- | Changes the permissions of a file.
chmod
  :: FilePath
  -> Perms
  -> Callback1 Unit
  -> Effect Unit
chmod file perms cb = runEffectFn3 chmodImpl file (permsToString perms) (handleCallback1 cb)

-- | Gets file statistics.
stat
  :: FilePath
  -> Callback1 Stats
  -> Effect Unit
stat file cb = runEffectFn2 statImpl file (handleCallback1 $ cb)

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat
  :: FilePath
  -> Callback1 Stats
  -> Effect Unit
lstat file cb = runEffectFn2 lstatImpl file (handleCallback1 $ cb)

-- | Creates a link to an existing file.
link
  :: FilePath
  -> FilePath
  -> Callback1 Unit
  -> Effect Unit
link src dst cb = runEffectFn3 linkImpl src dst (handleCallback1 cb)

-- | Creates a symlink.
symlink
  :: FilePath
  -> FilePath
  -> SymlinkType
  -> Callback1 Unit
  -> Effect Unit
symlink src dest ty cb = runEffectFn4 symlinkImpl src dest (symlinkTypeToNode ty) (handleCallback1 cb)

-- | Reads the value of a symlink.
readlink
  :: FilePath
  -> Callback1 FilePath
  -> Effect Unit
readlink path cb = runEffectFn2 readlinkImpl path (handleCallback1 cb)

-- | Find the canonicalized absolute location for a path.
realpath
  :: FilePath
  -> Callback1 FilePath
  -> Effect Unit
realpath path = realpath' path realpathOptionsDefault

-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
realpath'
  :: FilePath
  -> RealpathOptions
  -> Callback1 FilePath
  -> Effect Unit
realpath' path options cb = runEffectFn3 realpathImpl path (realpathOptionsToInternal options) (handleCallback1 cb)

-- | Deletes a file.
unlink
  :: FilePath
  -> Callback1 Unit
  -> Effect Unit
unlink file cb = runEffectFn2 unlinkImpl file (handleCallback1 cb)

-- | Deletes a directory.
rmdir
  :: FilePath
  -> Callback1 Unit
  -> Effect Unit
rmdir path cb = rmdir' path rmdirOptionsDefault cb

-- | Deletes a directory with options.
rmdir'
  :: FilePath
  -> RmdirOptions
  -> Callback1 Unit
  -> Effect Unit
rmdir' path opts cb = runEffectFn3 rmdirImpl path opts (handleCallback1 cb)

-- | Deletes a file or directory.
rm
  :: FilePath
  -> Callback1 Unit
  -> Effect Unit
rm path = rm' path rmOptionsDefault

-- | Deletes a file or directory with options.
rm'
  :: FilePath
  -> RmOptions
  -> Callback1 Unit
  -> Effect Unit
rm' path opts cb = runEffectFn3 rmImpl path opts (handleCallback1 cb)

-- | Makes a new directory.
mkdir
  :: FilePath
  -> Callback1 Unit
  -> Effect Unit
mkdir path = mkdir' path mkdirOptionsDefault

-- | Makes a new directory with the specified permissions.
mkdir'
  :: FilePath
  -> MkdirOptions
  -> Callback1 Unit
  -> Effect Unit
mkdir' file opts cb = runEffectFn3 mkdirImpl file (mkdirOptionsToInternal opts) (handleCallback1 cb)

-- | Reads the contents of a directory.
readdir
  :: FilePath
  -> Callback1 (Array FilePath)
  -> Effect Unit
readdir file = readdir' file readdirFilePathOptionsDefault

-- | Reads the contents of a directory.
readdir'
  :: FilePath
  -> ReaddirFilePathOptions
  -> Callback1 (Array FilePath)
  -> Effect Unit
readdir' file options cb = runEffectFn3 readdirImpl file (readdirFilePathOptionsToInternal options) (handleCallback1 cb)

-- | Reads the contents of a directory.
readdirBuffer
  :: FilePath
  -> Callback1 (Array Buffer)
  -> Effect Unit
readdirBuffer file = readdirBuffer' file readdirBufferOptionsDefault

-- | Reads the contents of a directory.
readdirBuffer'
  :: FilePath
  -> ReaddirBufferOptions
  -> Callback1 (Array Buffer)
  -> Effect Unit
readdirBuffer' file options cb = runEffectFn3 readdirImpl file (readdirBufferOptionsToInternal options) (handleCallback1 cb)

-- | Reads the contents of a directory.
readdirDirent
  :: FilePath
  -> Callback1 (Array (Dirent DirentNameTypeString))
  -> Effect Unit
readdirDirent file = readdirDirent' file readdirDirentOptionsDefault

-- | Reads the contents of a directory.
readdirDirent'
  :: FilePath
  -> ReaddirDirentOptions
  -> Callback1 (Array (Dirent DirentNameTypeString))
  -> Effect Unit
readdirDirent' file options cb = runEffectFn3 readdirImpl file (readdirDirentOptionsToInternal options) (handleCallback1 cb)

-- | Reads the contents of a directory.
readdirDirentBuffer
  :: FilePath
  -> Callback1 (Array (Dirent DirentNameTypeBuffer))
  -> Effect Unit
readdirDirentBuffer file = readdirDirentBuffer' file readdirDirentBufferOptionsDefault

-- | Reads the contents of a directory.
readdirDirentBuffer'
  :: FilePath
  -> ReaddirDirentBufferOptions
  -> Callback1 (Array (Dirent DirentNameTypeBuffer))
  -> Effect Unit
readdirDirentBuffer' file options cb = runEffectFn3 readdirImpl file (readdirDirentBufferOptionsToInternal options) (handleCallback1 cb)

-- | Sets the accessed and modified times for the specified file.
utimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Callback1 Unit
  -> Effect Unit
utimes file atime mtime cb = runEffectFn4 utimesImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime) (handleCallback1 cb)

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile
  :: Encoding
  -> FilePath
  -> Callback1 String
  -> Effect Unit
readTextFile encoding file = readTextFile' file (readFileStringOptionsDefault { encoding = encoding })

readTextFile'
  :: FilePath
  -> ReadFileStringOptions
  -> Callback1 String
  -> Effect Unit
readTextFile' file options cb = runEffectFn3 readFileImpl file (readFileStringOptionsToInternal options) (handleCallback1 cb)

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile
  :: FilePath
  -> Callback1 Buffer
  -> Effect Unit
readFile file = readFile' file readFileBufferOptionsDefault

readFile'
  :: FilePath
  -> ReadFileBufferOptions
  -> Callback1 Buffer
  -> Effect Unit
readFile' file options cb = runEffectFn3 readFileImpl file (readFileBufferOptionsToInternal options) (handleCallback1 cb)

-- | Writes text to a file using the specified encoding.
writeTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Callback1 Unit
  -> Effect Unit
writeTextFile encoding file buff = writeTextFile' file buff (writeFileStringOptionsDefault { encoding = encoding })

writeTextFile'
  :: FilePath
  -> String
  -> WriteFileStringOptions
  -> Callback1 Unit
  -> Effect Unit
writeTextFile' file buff options cb = runEffectFn4 writeFileImpl file buff (writeFileStringOptionsToInternal options) (handleCallback1 cb)

-- | Writes a buffer to a file.
writeFile
  :: FilePath
  -> Buffer
  -> Callback1 Unit
  -> Effect Unit
writeFile file buff = writeFile' file buff writeFileBufferOptionsDefault

writeFile'
  :: FilePath
  -> Buffer
  -> WriteFileBufferOptions
  -> Callback1 Unit
  -> Effect Unit
writeFile' file buff options cb = runEffectFn4 writeFileImpl file buff (writeFileBufferOptionsToInternal options) (handleCallback1 cb)

-- | Appends text to a file using the specified encoding.
appendTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Callback1 Unit
  -> Effect Unit
appendTextFile encoding file buff = appendTextFile' file buff (appendFileStringOptionsDefault { encoding = encoding }) 

appendTextFile'
  :: FilePath
  -> String
  -> AppendFileStringOptions
  -> Callback1 Unit
  -> Effect Unit
appendTextFile' file buff options cb = runEffectFn4 appendFileImpl file buff (appendFileStringOptionsToInternal options) (handleCallback1 cb)

-- | Appends a buffer to a file.
appendFile
  :: FilePath
  -> Buffer
  -> Callback1 Unit
  -> Effect Unit
appendFile file buff = appendFile' file buff appendFileBufferOptionsDefault

appendFile'
  :: FilePath
  -> Buffer
  -> AppendFileBufferOptions
  -> Callback1 Unit
  -> Effect Unit
appendFile' file buff options cb = runEffectFn4 appendFileImpl file buff (appendFileBufferOptionsToInternal options) (handleCallback1 cb)

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen
  :: FilePath
  -> FileFlags -- default 'r'
  -> Maybe FileMode -- default '0o666', TODO: use Perms?
  -> Callback1 FileDescriptor
  -> Effect Unit
fdOpen file flags mode cb = runEffectFn4 openImpl file (fileFlagsToNode flags) (toNullable mode) (handleCallback1 cb)

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength -- TODO: should be Maybe BufferLength
  -> Maybe FilePosition -- If position is null or -1 , data will be read from the current file position
  -> Callback1 ByteCount
  -> Effect Unit
fdRead fd buff off len pos cb = runEffectFn6 readImpl fd buff off len (toNullable pos) (handleCallback1 cb)

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback)
-- | for details.
fdRead'
  :: FileDescriptor
  -> FdReadOptions
  -> Callback1 (Tuple ByteCount Buffer)
  -> Effect Unit
fdRead' fd options cb = runEffectFn3 readWithOptionsImpl fd (fdReadOptionsToInternal options) (handleCallback1Tuple cb)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext
  :: FileDescriptor
  -> Buffer
  -> Callback1 ByteCount
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
  -> BufferLength -- TODO: should be Maybe BufferLength
  -> Maybe FilePosition
  -> Callback1 ByteCount
  -> Effect Unit
fdWrite fd buff off len pos cb = runEffectFn6 writeImpl fd buff off len (toNullable pos) (handleCallback1 cb)

-- | Write from a file asynchronously. See the [Node Documentation](https://nodejs.org/docs/latest/api/fs.html#fswritefd-options-callback)
-- | for details.
fdWrite'
  :: FileDescriptor
  -> FdWriteOptions
  -> Callback1 (Tuple ByteCount Buffer)
  -> Effect Unit
fdWrite' fd options cb = runEffectFn3 writeWithOptionsImpl fd (fdWriteOptionsToInternal options) (handleCallback1Tuple cb)

-- It is unsafe to use fs.write() multiple times on the same file without waiting for the callback. For this scenario, fs.createWriteStream() is recommended.
fdWriteString
  :: FileDescriptor
  -> String
  -> Maybe FilePosition
  -> Encoding
  -> Callback1 (Tuple ByteCount String)
  -> Effect Unit
fdWriteString fd string pos encoding cb = runEffectFn5 writeStringImpl fd string (toNullable pos) (encodingToNode encoding) (handleCallback1Tuple cb)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend
  :: FileDescriptor
  -> Buffer
  -> Callback1 ByteCount
  -> Effect Unit
fdAppend fd buff cb = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing cb

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose
  :: FileDescriptor
  -> Callback1 Unit
  -> Effect Unit
fdClose fd cb = runEffectFn2 closeImpl fd (handleCallback1 cb)

-- | Copy a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fspromises_copyfile_src_dest_mode)
-- | for details.
cp :: FilePath -> FilePath -> Callback1 Unit -> Effect Unit
cp src dest = cp' src dest cpOptionsDefault

cp' :: FilePath -> FilePath -> CpOptions -> Callback1 Unit -> Effect Unit
cp' src dest opts cb = runEffectFn4 cpImpl src dest (cpOptionsToCpOptionsInternal opts) (handleCallback1 cb)

-- | Change permissions on a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchmod_fd_mode_callback)
-- | for details.
fchmod :: FileDescriptor -> Perms -> Callback1 Unit -> Effect Unit
fchmod fd perms cb = runEffectFn3 fchmodImpl fd (permsToString perms) (handleCallback1 cb)

-- | Change ownership of a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchown_fd_uid_gid_callback)
-- | for details.
fchown :: FileDescriptor -> Int -> Int -> Callback1 Unit -> Effect Unit
fchown fd uid gid cb = runEffectFn4 fchownImpl fd uid gid (handleCallback1 cb)

-- | Synchronize a file's in-core state with storage. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fdatasync_fd_callback)
-- | for details.
fdatasync :: FileDescriptor -> Callback1 Unit -> Effect Unit
fdatasync fd cb = runEffectFn2 fdatasyncImpl fd (handleCallback1 cb)

-- | Get file status information. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fstat_fd_callback)
-- | for details.
fstat :: FileDescriptor -> Callback1 Stats -> Effect Unit
fstat fd cb = runEffectFn2 fstatImpl fd (handleCallback1 cb)

-- | Flushes a file descriptor to disk. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fsync_fd_callback)
-- | for details.
fsync :: FileDescriptor -> Callback1 Unit -> Effect Unit
fsync fd cb = runEffectFn2 fsyncImpl fd (handleCallback1 cb)

-- | Truncate a file to a specified length. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_ftruncate_fd_len_callback)
-- | for details.
ftruncate :: FileDescriptor -> Int -> Callback1 Unit -> Effect Unit
ftruncate fd len cb = runEffectFn3 ftruncateImpl fd len (handleCallback1 cb)

-- | Change file timestamps for a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_futimes_fd_atime_mtime_callback)
-- | for details.
futimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Callback1 Unit
  -> Effect Unit
futimes file atime mtime cb = runEffectFn4 lutimesImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime) (handleCallback1 cb)

-- | Perform pattern matching in file paths. See the [Node Documentation](https://nodejs.org/api/glob.html#globglob_pattern_options_callback)
-- | for details.
glob :: Array FilePath -> Callback1 (Array FilePath) -> Effect Unit
glob pattern = glob' pattern globFilePathOptionsDefault

glob' :: Array FilePath -> GlobFilePathOptions -> Callback1 (Array FilePath) -> Effect Unit
glob' pattern options cb = runEffectFn3 globImpl pattern (globFilePathOptionsToInternal options) (handleCallback1 cb)

globDirent :: Array FilePath -> Callback1 (Array (Dirent DirentNameTypeString)) -> Effect Unit
globDirent pattern = globDirent' pattern globDirentOptionsDefault

globDirent' :: Array FilePath -> GlobDirentOptions -> Callback1 (Array (Dirent DirentNameTypeString)) -> Effect Unit
globDirent' pattern options cb = runEffectFn3 globImpl pattern (globDirentOptionsToInternal options) (handleCallback1 cb)

-- | Change permissions on a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchmod_path_mode_callback)
-- | for details.
lchmod :: FilePath -> Perms -> Callback1 Unit -> Effect Unit
lchmod path perms cb = runEffectFn3 lchmodImpl path (permsToString perms) (handleCallback1 cb)

-- | Change ownership of a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchown_path_uid_gid_callback)
-- | for details.
lchown :: FilePath -> Int -> Int -> Callback1 Unit -> Effect Unit
lchown path uid gid cb = runEffectFn4 lchownImpl path uid gid (handleCallback1 cb)

-- | Change timestamps for a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lutimes_path_atime_mtime_callback)
-- | for details.
lutimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Callback1 Unit
  -> Effect Unit
lutimes file atime mtime cb = runEffectFn4 lutimesImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime) (handleCallback1 cb)

-- | TODO: path - Buffer Url, returns Promise
-- | Open a file as a blob. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_class_filehandle)
-- | for details.
-- openAsBlob :: FilePath -> Promise Blob -> Effect Unit
-- openAsBlob path cb = runEffectFn2 openAsBlobImpl path (handleCallback1 cb)

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
opendir' :: FilePath -> OpendirOptions -> Callback1 Dir -> Effect Unit
opendir' path options cb = runEffectFn3 opendirImpl path (opendirOptionsToInternal options) (handleCallback1 cb)

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
-- | NOTE: encoding: 'buffer' is not supported, will throw error "TypeError [ERR_INVALID_ARG_TYPE]: The "path" argument must be of type string. Received an instance of Buffer"
opendir :: FilePath -> Callback1 Dir -> Effect Unit
opendir path = opendir' path opendirOptionsDefault

-- | Read from a file descriptor into a buffer array. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_readv_fd_buffers_position_callback)
-- | for details.
readv :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Callback1 (Tuple ByteCount (Array Buffer)) -> Effect Unit
readv fd buffers position cb = runEffectFn4 readvImpl fd buffers (toNullable position) (handleCallback1Tuple cb)

-- | Get file system statistics. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_statfs_path_callback)
-- | for details.
statfs :: FilePath -> Callback1 Stats -> Effect Unit
statfs path cb = runEffectFn2 statfsImpl path (handleCallback1 cb)

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
writev :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Callback1 (Tuple ByteCount (Array Buffer)) -> Effect Unit
writev fd buffers position cb = runEffectFn4 writevImpl fd buffers (toNullable position) (handleCallback1Tuple cb)
