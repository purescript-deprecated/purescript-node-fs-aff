module Node.FS.Aff
  ( rename
  , truncate
  , chown
  , chmod
  , stat
  , link
  , symlink
  , readlink
  , realpath
  , realpath'
  , unlink
  , rmdir
  , mkdir
  , mkdir'
  , readdir
  , utimes
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
  , appendFile
  , appendTextFile
  , exists
  , fdOpen
  , fdRead
  , fdNext
  , fdWrite
  , fdAppend
  , fdClose
  , module Exports
  ) where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Maybe (Maybe)
import Node.Buffer (Buffer, BUFFER)
import Node.Encoding (Encoding)
import Node.FS as F
import Node.FS.Async as A
import Node.FS.Perms (Perms)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)

import Node.FS (FS) as Exports

toAff :: forall eff a.
  (A.Callback eff a -> Eff (fs :: F.FS | eff) Unit) ->
  Aff (fs :: F.FS | eff) a
toAff p = makeAff \e a -> p $ either e a

toAff1 :: forall eff a x.
  (x -> A.Callback eff a -> Eff (fs :: F.FS | eff) Unit) ->
  x ->
  Aff (fs :: F.FS | eff) a
toAff1 f a     = toAff (f a)

toAff2 :: forall eff a x y.
  (x -> y -> A.Callback eff a -> Eff (fs :: F.FS | eff) Unit) ->
  x ->
  y ->
  Aff (fs :: F.FS | eff) a
toAff2 f a b   = toAff (f a b)

toAff3 :: forall eff a x y z.
  (x -> y -> z -> A.Callback eff a -> Eff (fs :: F.FS | eff) Unit) ->
  x ->
  y ->
  z ->
  Aff (fs :: F.FS | eff) a
toAff3 f a b c = toAff (f a b c)

toAff5 :: forall eff a w v x y z.
  (w -> v -> x -> y -> z -> A.Callback eff a -> Eff (fs :: F.FS | eff) Unit) ->
  w ->
  v ->
  x ->
  y ->
  z ->
  Aff (fs :: F.FS | eff) a
toAff5 f a b c d e = toAff (f a b c d e)

-- |
-- | Rename a file.
-- |
rename :: forall eff. FilePath
                   -> FilePath
                   -> Aff (fs :: F.FS | eff) Unit
rename = toAff2 A.rename

-- |
-- | Truncates a file to the specified length.
-- |
truncate :: forall eff. FilePath
                     -> Int
                     -> Aff (fs :: F.FS | eff) Unit
truncate = toAff2 A.truncate

-- |
-- | Changes the ownership of a file.
-- |
chown :: forall eff. FilePath
                  -> Int
                  -> Int
                  -> Aff (fs :: F.FS | eff) Unit
chown = toAff3 A.chown

-- |
-- | Changes the permissions of a file.
-- |
chmod :: forall eff. FilePath
                  -> Perms
                  -> Aff (fs :: F.FS | eff) Unit
chmod = toAff2 A.chmod

-- |
-- | Gets file statistics.
-- |
stat :: forall eff. FilePath
                 -> Aff (fs :: F.FS | eff) Stats
stat = toAff1 A.stat

-- |
-- | Creates a link to an existing file.
-- |
link :: forall eff. FilePath
                 -> FilePath
                 -> Aff (fs :: F.FS | eff) Unit
link = toAff2 A.link

-- |
-- | Creates a symlink.
-- |
symlink :: forall eff. FilePath
                    -> FilePath
                    -> F.SymlinkType
                    -> Aff (fs :: F.FS | eff) Unit
symlink = toAff3 A.symlink

-- |
-- | Reads the value of a symlink.
-- |
readlink :: forall eff. FilePath
                     -> Aff (fs :: F.FS | eff) FilePath
readlink = toAff1 A.readlink

-- |
-- | Find the canonicalized absolute location for a path.
-- |
realpath :: forall eff. FilePath
                     -> Aff (fs :: F.FS | eff) FilePath
realpath = toAff1 A.realpath

-- |
-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
-- |
realpath' :: forall eff cache. FilePath
                            -> { | cache }
                            -> Aff (fs :: F.FS | eff) FilePath
realpath' = toAff2 A.realpath'

-- |
-- | Deletes a file.
-- |
unlink :: forall eff. FilePath
                   -> Aff (fs :: F.FS | eff) Unit
unlink = toAff1 A.unlink

-- |
-- | Deletes a directory.
-- |
rmdir :: forall eff. FilePath
                   -> Aff (fs :: F.FS | eff) Unit
rmdir = toAff1 A.rmdir

-- |
-- | Makes a new directory.
-- |
mkdir :: forall eff. FilePath
                  -> Aff (fs :: F.FS | eff) Unit
mkdir = toAff1 A.mkdir

-- |
-- | Makes a new directory with the specified permissions.
-- |
mkdir' :: forall eff. FilePath
                   -> Perms
                   -> Aff (fs :: F.FS | eff) Unit
mkdir' = toAff2 A.mkdir'

-- |
-- | Reads the contents of a directory.
-- |
readdir :: forall eff. FilePath
                    -> Aff (fs :: F.FS | eff) (Array FilePath)
readdir = toAff1 A.readdir

-- |
-- | Sets the accessed and modified times for the specified file.
-- |
utimes :: forall eff. FilePath
                   -> DateTime
                   -> DateTime
                   -> Aff (fs :: F.FS | eff) Unit
utimes = toAff3 A.utimes

-- |
-- | Reads the entire contents of a file returning the result as a raw buffer.
-- |
readFile :: forall eff. FilePath
                     -> Aff (fs :: F.FS, buffer :: BUFFER | eff) Buffer
readFile = toAff1 A.readFile

-- |
-- | Reads the entire contents of a text file with the specified encoding.
-- |
readTextFile :: forall eff. Encoding
                         -> FilePath
                         -> Aff (fs :: F.FS | eff) String
readTextFile = toAff2 A.readTextFile

-- |
-- | Writes a buffer to a file.
-- |
writeFile :: forall eff. FilePath
                      -> Buffer
                      -> Aff (fs :: F.FS, buffer :: BUFFER | eff) Unit
writeFile = toAff2 A.writeFile

-- |
-- | Writes text to a file using the specified encoding.
-- |
writeTextFile :: forall eff. Encoding
                          -> FilePath
                          -> String
                          -> Aff (fs :: F.FS | eff) Unit
writeTextFile = toAff3 A.writeTextFile

-- |
-- | Appends the contents of a buffer to a file.
-- |
appendFile :: forall eff. FilePath
                       -> Buffer
                       -> Aff (fs :: F.FS, buffer :: BUFFER | eff) Unit
appendFile = toAff2 A.appendFile

-- |
-- | Appends text to a file using the specified encoding.
-- |
appendTextFile :: forall eff. Encoding
                           -> FilePath
                           -> String
                           -> Aff (fs :: F.FS | eff) Unit
appendTextFile = toAff3 A.appendTextFile

-- |
-- | Check to see if a file exists.
-- |
exists :: forall eff. String
                   -> Aff (fs :: F.FS | eff) Boolean
exists file = makeAff \_ a -> A.exists file a

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen :: forall eff.
          FilePath
       -> F.FileFlags
       -> Maybe F.FileMode
       -> Aff (fs :: F.FS | eff) F.FileDescriptor
fdOpen = toAff3 A.fdOpen

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead :: forall eff.
          F.FileDescriptor
       -> Buffer
       -> F.BufferOffset
       -> F.BufferLength
       -> Maybe F.FilePosition
       -> Aff (buffer :: BUFFER, fs :: F.FS | eff) F.ByteCount
fdRead = toAff5 A.fdRead

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext :: forall eff.
          F.FileDescriptor
       -> Buffer
       -> Aff (buffer :: BUFFER, fs :: F.FS | eff) F.ByteCount
fdNext = toAff2 A.fdNext

-- | Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
-- | for details.
fdWrite :: forall eff.
           F.FileDescriptor
        -> Buffer
        -> F.BufferOffset
        -> F.BufferLength
        -> Maybe F.FilePosition
        -> Aff (buffer :: BUFFER, fs :: F.FS | eff) F.ByteCount
fdWrite = toAff5 A.fdWrite

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend :: forall eff.
            F.FileDescriptor
         -> Buffer
         -> Aff (buffer :: BUFFER, fs :: F.FS | eff) F.ByteCount
fdAppend = toAff2 A.fdAppend

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose :: forall eff.
           F.FileDescriptor
        -> Aff (fs :: F.FS | eff) Unit
fdClose = toAff1 A.fdClose
