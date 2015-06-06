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
  ) where

import Node.Path (FilePath())
import Node.FS.Perms (Perms())
import Node.FS.Stats (Stats())
import Data.Date (Date())
import Control.Monad.Eff (Eff(), runPure)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Data.Either (either)
import Control.Monad.Aff (Aff(), makeAff)
import Node.Buffer (Buffer())
import Node.Encoding (Encoding())
import qualified Node.FS as F
import qualified Node.FS.Async as A

toAff :: forall eff a.
  (A.Callback eff a -> Eff (fs :: F.FS | eff) Unit) ->
  Aff (fs :: F.FS | eff) a
toAff p = makeAff \e a -> p $ either e a

toAff1 f a     = toAff (f a)
toAff2 f a b   = toAff (f a b)
toAff3 f a b c = toAff (f a b c)

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
                     -> Number
                     -> Aff (fs :: F.FS | eff) Unit
truncate = toAff2 A.truncate

-- |
-- | Changes the ownership of a file.
-- |
chown :: forall eff. FilePath
                  -> Number
                  -> Number
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
                    -> Aff (fs :: F.FS | eff) [FilePath]
readdir = toAff1 A.readdir

-- |
-- | Sets the accessed and modified times for the specified file.
-- |
utimes :: forall eff. FilePath
                   -> Date
                   -> Date
                   -> Aff (fs :: F.FS | eff) Unit
utimes = toAff3 A.utimes

-- |
-- | Reads the entire contents of a file returning the result as a raw buffer.
-- |
readFile :: forall eff. FilePath
                     -> Aff (fs :: F.FS | eff) Buffer
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
                      -> Aff (fs :: F.FS | eff) Unit
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
                       -> Aff (fs :: F.FS | eff) Unit
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
-- Patch `Node.FS.Async.exists`
-- The current version of `Node.FS.Async.exists` fails the occurs check
-- because it's callback signature does not include the FS effect.
--
import Data.Function
foreign import mkEff
  "function mkEff(action) {\
  \  return action;\
  \}" :: forall eff a. (Unit -> a) -> Eff eff a
foreign import fs "var fs = require('fs');" ::
  { exists :: forall a. Fn2 FilePath (Boolean -> a) Unit }
_exists file cb = mkEff $ \_ -> runFn2
  fs.exists file $ \b -> runPure (unsafeInterleaveEff (cb b))

-- |
-- | Check to see if a file exists.
-- |
exists :: forall eff. String
                   -> Aff (fs :: F.FS | eff) Boolean
exists file = makeAff \_ a -> _exists file a
