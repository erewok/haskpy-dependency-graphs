module DependencyGraph.GraphModules (
  Graph,
  graphModule
  ) where

import Control.Monad
import DependencyGraph.Modules as M


data Graph = Graph { head :: FilePath
                   , nodes :: IO [FilePath]}



graphModule :: M.Environment -> FilePath -> Graph
graphModule env infile = do
  let paths = M.findAllModules env infile
  Graph infile paths

-- Walk directory: http://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
-- check file exists: http://rosettacode.org/wiki/Check_that_file_exists#Haskell
-- See also: Chapter 18 of Real World Haskell
