module Main where

import Prelude
import System.Environment
import System.FilePath (splitSearchPath)

import qualified DependencyGraph as DG

main :: IO ()
main = do
  (pyvers:infile:[]) <- getArgs
  pypath <- getEnv "PYTHONPATH"
  let env = DG.Environment pyvers (splitSearchPath pypath)
  let results = DG.startGraph env infile
  DG.displayGraph results
