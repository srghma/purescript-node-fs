module Node.FS.Aff
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
  , stat
  , lstat
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
  , module Exports
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.FS as F
import Node.FS.Async (CpOptions)
import Node.FS.Async (CpOptions, CpForce(..), cpOptionsDefault) as Exports
import Node.FS.Async as A
import Node.FS.Constants (AccessMode, CopyMode)
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameTypeBuffer, DirentNameTypeString)
import Node.FS.Perms (Perms)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)

toAff
  :: forall a
   . (A.Callback a -> Effect Unit)
  -> Aff a
toAff p = makeAff \k -> p k $> nonCanceler

toAff1
  :: forall a x
   . (x -> A.Callback a -> Effect Unit)
  -> x
  -> Aff a
toAff1 f a = toAff (f a)

toAff2
  :: forall a x y
   . (x -> y -> A.Callback a -> Effect Unit)
  -> x
  -> y
  -> Aff a
toAff2 f a b = toAff (f a b)

toAff3
  :: forall a x y z
   . (x -> y -> z -> A.Callback a -> Effect Unit)
  -> x
  -> y
  -> z
  -> Aff a
toAff3 f a b c = toAff (f a b c)

-- toAff4
--   :: forall a x y z
--    . (x -> y -> z -> y -> A.Callback a -> Effect Unit)
--   -> x
--   -> y
--   -> z
--   -> y
--   -> Aff a
-- toAff4 f a b c d = toAff (f a b c d)

toAff5
  :: forall a w v x y z
   . (w -> v -> x -> y -> z -> A.Callback a -> Effect Unit)
  -> w
  -> v
  -> x
  -> y
  -> z
  -> Aff a
toAff5 f a b c d e = toAff (f a b c d e)

-- toAff6
--   :: forall a w v x y z t
--    . (w -> v -> x -> y -> z -> t -> A.Callback a -> Effect Unit)
--   -> w
--   -> v
--   -> x
--   -> y
--   -> z
--   -> t
--   -> Aff a
-- toAff6 f a b c d e t = toAff (f a b c d e t)

access :: FilePath -> Aff (Maybe Error)
access path = makeAff \k -> do
  A.access path (k <<< Right)
  pure nonCanceler

access' :: FilePath -> AccessMode -> Aff (Maybe Error)
access' path mode = makeAff \k -> do
  A.access' path mode (k <<< Right)
  pure nonCanceler

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile = toAff2 A.copyFile

copyFile' :: FilePath -> FilePath -> CopyMode -> Aff Unit
copyFile' = toAff3 A.copyFile'

mkdtemp :: FilePath -> Aff FilePath
mkdtemp = toAff1 A.mkdtemp

mkdtemp' :: FilePath -> Encoding -> Aff FilePath
mkdtemp' = toAff2 A.mkdtemp'

-- |
-- | Rename a file.
-- |
rename :: FilePath -> FilePath -> Aff Unit
rename = toAff2 A.rename

-- |
-- | Truncates a file to the specified length.
-- |
truncate :: FilePath -> Int -> Aff Unit
truncate = toAff2 A.truncate

-- |
-- | Changes the ownership of a file.
-- |
chown :: FilePath -> Int -> Int -> Aff Unit
chown = toAff3 A.chown

-- |
-- | Changes the permissions of a file.
-- |
chmod :: FilePath -> Perms -> Aff Unit
chmod = toAff2 A.chmod

-- |
-- | Gets file statistics.
-- |
stat :: FilePath -> Aff Stats
stat = toAff1 A.stat

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat :: FilePath -> Aff Stats
lstat = toAff1 A.lstat

-- |
-- | Creates a link to an existing file.
-- |
link :: FilePath -> FilePath -> Aff Unit
link = toAff2 A.link

-- |
-- | Creates a symlink.
-- |
symlink
  :: FilePath
  -> FilePath
  -> F.SymlinkType
  -> Aff Unit
symlink = toAff3 A.symlink

-- |
-- | Reads the value of a symlink.
-- |
readlink :: FilePath -> Aff FilePath
readlink = toAff1 A.readlink

-- |
-- | Find the canonicalized absolute location for a path.
-- |
realpath :: FilePath -> Aff FilePath
realpath = toAff1 A.realpath

-- |
-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
-- |
realpath' :: forall cache. FilePath -> { | cache } -> Aff FilePath
realpath' = toAff2 A.realpath'

-- |
-- | Deletes a file.
-- |
unlink :: FilePath -> Aff Unit
unlink = toAff1 A.unlink

-- |
-- | Deletes a directory.
-- |
rmdir :: FilePath -> Aff Unit
rmdir = toAff1 A.rmdir

-- |
-- | Deletes a directory with options.
-- |
rmdir' :: FilePath -> { maxRetries :: Int, retryDelay :: Int } -> Aff Unit
rmdir' = toAff2 A.rmdir'

-- |
-- | Deletes a file or directory.
-- |
rm :: FilePath -> Aff Unit
rm = toAff1 A.rm

-- |
-- | Deletes a file or directory with options.
-- |
rm' :: FilePath -> { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int } -> Aff Unit
rm' = toAff2 A.rm'

-- |
-- | Makes a new directory.
-- |
mkdir :: FilePath -> Aff Unit
mkdir = toAff1 A.mkdir

-- |
-- | Makes a new directory with all of its options.
-- |
mkdir' :: FilePath -> { recursive :: Boolean, mode :: Perms } -> Aff Unit
mkdir' = toAff2 A.mkdir'

-- |
-- | Reads the contents of a directory.
-- |
readdir :: FilePath -> Aff (Array FilePath)
readdir = toAff1 A.readdir

-- | Reads the contents of a directory with options.
readdir' :: FilePath -> { recursive :: Boolean, encoding :: Encoding } -> Aff (Array FilePath)
readdir' = toAff2 A.readdir'

-- | Reads the contents of a directory and returns an Aff (Array Buffer).
readdirBuffer :: FilePath -> Aff (Array Buffer)
readdirBuffer = toAff1 A.readdirBuffer

-- | Reads the contents of a directory with options and returns Aff (Array Buffer).
readdirBuffer' :: FilePath -> { recursive :: Boolean } -> Aff (Array Buffer)
readdirBuffer' = toAff2 A.readdirBuffer'

-- | Reads the contents of a directory and returns an Aff (Array (Dirent DirentNameTypeString)).
readdirDirent :: FilePath -> Aff (Array (Dirent DirentNameTypeString))
readdirDirent = toAff1 A.readdirDirent

-- | Reads the contents of a directory with options and returns Aff (Array (Dirent DirentNameTypeString)).
readdirDirent' :: FilePath -> { recursive :: Boolean, encoding :: Encoding } -> Aff (Array (Dirent DirentNameTypeString))
readdirDirent' = toAff2 A.readdirDirent'

-- | Reads the contents of a directory.
readdirDirentBuffer
  :: FilePath
  -> Aff (Array (Dirent DirentNameTypeBuffer))
readdirDirentBuffer = toAff1 A.readdirDirentBuffer

-- | Reads the contents of a directory.
readdirDirentBuffer'
  :: FilePath
  -> { recursive :: Boolean }
  -> Aff (Array (Dirent DirentNameTypeBuffer))
readdirDirentBuffer' = toAff2 A.readdirDirentBuffer'

-- |
-- | Sets the accessed and modified times for the specified file.
-- |
utimes :: FilePath -> DateTime -> DateTime -> Aff Unit
utimes = toAff3 A.utimes

-- |
-- | Reads the entire contents of a file returning the result as a raw buffer.
-- |
readFile :: FilePath -> Aff Buffer
readFile = toAff1 A.readFile

-- |
-- | Reads the entire contents of a text file with the specified encoding.
-- |
readTextFile :: Encoding -> FilePath -> Aff String
readTextFile = toAff2 A.readTextFile

-- |
-- | Writes a buffer to a file.
-- |
writeFile :: FilePath -> Buffer -> Aff Unit
writeFile = toAff2 A.writeFile

-- |
-- | Writes text to a file using the specified encoding.
-- |
writeTextFile :: Encoding -> FilePath -> String -> Aff Unit
writeTextFile = toAff3 A.writeTextFile

-- |
-- | Appends the contents of a buffer to a file.
-- |
appendFile :: FilePath -> Buffer -> Aff Unit
appendFile = toAff2 A.appendFile

-- |
-- | Appends text to a file using the specified encoding.
-- |
appendTextFile :: Encoding -> FilePath -> String -> Aff Unit
appendTextFile = toAff3 A.appendTextFile

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen
  :: FilePath
  -> F.FileFlags
  -> Maybe F.FileMode
  -> Aff F.FileDescriptor
fdOpen = toAff3 A.fdOpen

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead
  :: F.FileDescriptor
  -> Buffer
  -> F.BufferOffset
  -> F.BufferLength
  -> Maybe F.FilePosition
  -> Aff F.ByteCount
fdRead = toAff5 A.fdRead

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext :: F.FileDescriptor -> Buffer -> Aff F.ByteCount
fdNext = toAff2 A.fdNext

-- | Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
-- | for details.
fdWrite
  :: F.FileDescriptor
  -> Buffer
  -> F.BufferOffset
  -> F.BufferLength
  -> Maybe F.FilePosition
  -> Aff F.ByteCount
fdWrite = toAff5 A.fdWrite

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend :: F.FileDescriptor -> Buffer -> Aff F.ByteCount
fdAppend = toAff2 A.fdAppend

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose :: F.FileDescriptor -> Aff Unit
fdClose = toAff1 A.fdClose

-- | Copy a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fspromises_copyfile_src_dest_mode)
-- | for details.
cp :: FilePath -> FilePath -> Aff Unit
cp = toAff2 A.cp

cp' :: FilePath -> FilePath -> CpOptions -> Aff Unit
cp' = toAff3 A.cp'

-- | Change permissions on a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchmod_fd_mode_callback)
-- | for details.
fchmod :: F.FileDescriptor -> Perms -> Aff Unit
fchmod = toAff2 A.fchmod

-- | Change ownership of a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchown_fd_uid_gid_callback)
-- | for details.
fchown :: F.FileDescriptor -> Int -> Int -> Aff Unit
fchown = toAff3 A.fchown

-- | Synchronize a file's in-core state with storage. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fdatasync_fd_callback)
-- | for details.
fdatasync :: F.FileDescriptor -> Aff Unit
fdatasync = toAff1 A.fdatasync

-- | Get file status information. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fstat_fd_callback)
-- | for details.
fstat :: F.FileDescriptor -> Aff Stats
fstat = toAff1 A.fstat

-- | Flushes a file descriptor to disk. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fsync_fd_callback)
-- | for details.
fsync :: F.FileDescriptor -> Aff Unit
fsync = toAff1 A.fsync

-- | Truncate a file to a specified length. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_ftruncate_fd_len_callback)
-- | for details.
ftruncate :: F.FileDescriptor -> Int -> Aff Unit
ftruncate = toAff2 A.ftruncate

-- | Change file timestamps for a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_futimes_fd_atime_mtime_callback)
-- | for details.
futimes :: FilePath -> DateTime -> DateTime -> Aff Unit
futimes = toAff3 A.futimes

-- | Perform pattern matching in file paths. See the [Node Documentation](https://nodejs.org/api/glob.html#globglob_pattern_options_callback)
-- | for details.
glob :: Array FilePath -> Aff (Array FilePath)
glob = toAff1 A.glob

glob' :: Array FilePath -> { cwd :: Maybe FilePath, exclude :: Maybe (FilePath -> Boolean) } -> Aff (Array FilePath)
glob' = toAff2 A.glob'

globDirent :: Array FilePath -> Aff (Array (Dirent DirentNameTypeString))
globDirent = toAff1 A.globDirent

globDirent' :: Array FilePath -> { cwd :: Maybe FilePath, exclude :: Maybe (Dirent DirentNameTypeString -> Boolean) } -> Aff (Array (Dirent DirentNameTypeString))
globDirent' = toAff2 A.globDirent'

-- | Change permissions on a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchmod_path_mode_callback)
-- | for details.
lchmod :: FilePath -> Perms -> Aff Unit
lchmod = toAff2 A.lchmod

-- | Change ownership of a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchown_path_uid_gid_callback)
-- | for details.
lchown :: FilePath -> Int -> Int -> Aff Unit
lchown = toAff3 A.lchown

-- | Change timestamps for a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lutimes_path_atime_mtime_callback)
-- | for details.
lutimes :: FilePath -> DateTime -> DateTime -> Aff Unit
lutimes = toAff3 A.lutimes

-- | Open a file as a blob. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_class_filehandle)
-- | for details.
-- openAsBlob :: FilePath -> Aff Blob
-- openAsBlob = toAff1 A.openAsBlob

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
opendir :: FilePath -> Aff Dir
opendir = toAff1 A.opendir

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
opendir' :: FilePath -> { bufferSize :: Int, recursive :: Boolean, encoding :: Encoding } -> Aff Dir
opendir' = toAff2 A.opendir'

-- | Read from a file descriptor into a buffer array. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_readv_fd_buffers_position_callback)
-- | for details.
readv :: F.FileDescriptor -> Array Buffer -> Maybe F.FilePosition -> Aff (Tuple F.ByteCount (Array Buffer))
readv = toAff3 A.readv

-- | TODO: bigint, path Buffer Url
-- | Get file system statistics. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_statfs_path_callback)
-- | for details.
statfs :: FilePath -> Aff Stats
statfs = toAff1 A.statfs

-- | TODO: implement
-- | Stop watching a file for changes. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_unwatchfile_filename_listener)
-- | for details.
-- unwatchFile :: FilePath -> Effect Unit
-- unwatchFile = toAff1 A.unwatchFile

-- | Watch for changes in a file or directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_watch_filename_options_listener)
-- | for details.
-- watch :: FilePath -> (String -> Effect Unit) -> Effect Unit
-- watch = toAff2 A.watch

-- | Watch for changes in a file and trigger a callback. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_watchfile_filename_options_listener)
-- | for details.
-- watchFile :: FilePath -> (Stats -> Stats -> Effect Unit) -> Effect Unit
-- watchFile = toAff2 A.watchFile

-- | Write from an array of buffers to a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_writev_fd_buffers_position_callback)
-- | for details.
writev :: F.FileDescriptor -> Array Buffer -> Maybe F.FilePosition -> Aff (Tuple F.ByteCount (Array Buffer))
writev = toAff3 A.writev
