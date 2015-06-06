module Example.Main where

import Control.Monad (filterM)
import Control.Functor
import Data.String (charAt, fromChar)
import Data.Maybe
import Data.Array
import Control.Monad.Trans
import Control.Monad.Aff
import Control.Monad.Eff.Class (liftEff)
import qualified Node.Path as Path
import qualified Debug.Trace as T
import qualified Node.FS.Aff as FS
import qualified Node.FS as FS
import qualified Node.FS.Stats as FS

trace :: forall e a. (Show a) => a -> Aff (trace :: T.Trace | e) a
trace a = do
  liftEff $ T.trace (show a)
  return a

main = launchAff do
  files <- FS.readdir "."
  files' <- flip filterM files \file -> do
    stat <- FS.stat file
    return $
         FS.isDirectory stat
      && (maybe false (fromChar >>> (/= ".")) $ charAt 0 file)
  liftEff $ Debug.Trace.trace $ show files'
