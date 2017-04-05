module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filterA)
import Data.Maybe (maybe)
import Data.String (charAt, singleton)
import Node.FS (FS)
import Node.FS.Aff (stat, readdir)
import Node.FS.Stats (isDirectory)

main :: forall eff. Eff ( exception :: EXCEPTION
                        , fs        :: FS
                        , console   :: CONSOLE
                        | eff
                        ) Unit
main = void $ launchAff do
  files <- readdir "."
  files' <- flip filterA files \file -> do
    stat <- stat file
    pure $ isDirectory stat
      && (maybe false (singleton >>> (_ /= ".")) $ charAt 0 file)
  liftEff $ log $ show files'
