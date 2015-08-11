module Example.Main where

import Prelude

import Data.Maybe
import Data.Array
import Data.Functor
import Data.String (charAt, fromChar)

import Control.Monad.Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console

import qualified Node.Path as Path
import qualified Node.FS.Aff as FS
import qualified Node.FS as FS
import qualified Node.FS.Stats as FS

main = launchAff do
  files <- FS.readdir "."
  files' <- flip filterM files \file -> do
    stat <- FS.stat file
    return $
         FS.isDirectory stat
      && (maybe false (fromChar >>> (/= ".")) $ charAt 0 file)
  liftEff $ log $ show files'
