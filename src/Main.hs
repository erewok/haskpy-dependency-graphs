module Main where

import Control.Monad
import Data.List.Split
import Prelude
import System.Environment

import qualified DependencyGraph as DG

main :: IO ()
main = do
  (pyvers:infile:[]) <- getArgs
  pypath <- getEnv "PYTHONPATH"
  let env = DG.Environment pyvers (splitOn ":" pypath)
  modules <- DG.findAllModules env infile
  mapM_ putStrLn modules
