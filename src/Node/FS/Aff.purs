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

import Control.Monad.Eff (Eff(..))
import Data.Either (either)
import Control.Monad.Aff (Aff(..), makeAff)
import qualified Node.FS as F
import qualified Node.FS.Async as A

toAff :: forall eff a.
  (A.Callback eff a -> Eff (fs :: F.FS | eff) Unit) ->
  Aff (fs :: F.FS | eff) a
toAff p = makeAff (\e a -> p $ either e a)

toAff1 f a     = toAff (f a)
toAff2 f a b   = toAff (f a b)
toAff3 f a b c = toAff (f a b c)

rename         = toAff2 A.rename
truncate       = toAff2 A.truncate
chown          = toAff3 A.chown
chmod          = toAff2 A.chmod
stat           = toAff1 A.stat
link           = toAff2 A.link
symlink        = toAff3 A.symlink
readlink       = toAff1 A.readlink
realpath       = toAff1 A.realpath
realpath'      = toAff2 A.realpath'
unlink         = toAff1 A.unlink
rmdir          = toAff1 A.rmdir
mkdir          = toAff1 A.mkdir
mkdir'         = toAff2 A.mkdir'
readdir        = toAff1 A.readdir
utimes         = toAff3 A.utimes
readFile       = toAff1 A.readFile
readTextFile   = toAff2 A.readTextFile
writeFile      = toAff2 A.writeFile
writeTextFile  = toAff3 A.writeTextFile
appendFile     = toAff2 A.appendFile
appendTextFile = toAff3 A.appendTextFile

-- |
-- Patch `Node.FS.Async.exists`
--
import Data.Function
foreign import fs "var fs = require('fs');" ::
  { exists :: forall a. Fn2 String (Boolean -> a) Unit }

exists :: forall e. String -> Aff (fs :: F.FS | e) Boolean
exists file = makeAff \_ a -> pure $ runFn2 fs.exists file a
