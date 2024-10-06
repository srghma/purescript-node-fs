module Node.FS.Types where

import Prelude

import Data.Nullable (Nullable)
import Data.Nullable as Nullable

foreign import data FileDescriptor :: Type

type FileMode = Int
type FilePosition = Int
type BufferLength = Int
type BufferOffset = Int
type ByteCount = Int

-- | Symlink varieties.
data SymlinkType = FileLink | DirLink | JunctionLink | AutodetectLink

-- | Convert a `SymlinkType` to a `String` in the format expected by the
-- | Node.js filesystem API.
symlinkTypeToNode :: SymlinkType -> Nullable String
symlinkTypeToNode ty = case ty of
  FileLink -> Nullable.notNull "file"
  DirLink -> Nullable.notNull "dir"
  JunctionLink -> Nullable.notNull "junction"
  AutodetectLink -> Nullable.null

instance showSymlinkType :: Show SymlinkType where
  show FileLink = "FileLink"
  show DirLink = "DirLink"
  show JunctionLink = "JunctionLink"
  show AutodetectLink = "AutodetectLink"

derive instance eqSymlinkType :: Eq SymlinkType
