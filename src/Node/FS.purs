module Node.FS
  ( FileDescriptor
  , FileMode
  , SymlinkType(..)
  , symlinkTypeToNode
  , BufferLength
  , BufferOffset
  , ByteCount
  , FilePosition
  , module Exports
  ) where

import Prelude

import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Node.FS.Constants (FileFlags(..), fileFlagsToNode) as Exports

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
