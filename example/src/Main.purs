module Main where

import Prelude

import Data.List (filterM)
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..))
import Data.String.Utils (charAt)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.FS.Aff as FSA
import Node.FS.Stats as FS
import Node.Path (FilePath)

isDotFile :: FilePath -> Boolean
isDotFile filepath = (charAt 0 filepath) == Just "."

isHiddenDirectory :: FilePath -> Aff Boolean
isHiddenDirectory filepath = do
  stat <- FSA.stat filepath
  pure $ FS.isDirectory stat && not (isDotFile filepath)
       
main :: Effect Unit
main = launchAff_ do
  files <- FSA.readdir "."
  files' <- filterM isHiddenDirectory (L.fromFoldable files) 
  liftEffect $ traverse log files'
