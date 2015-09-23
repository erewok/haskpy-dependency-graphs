module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (reader, runReaderT)
import System.Environment
import System.FilePath (splitSearchPath)

import qualified DependencyGraph as DG


main :: IO ()
main = do
  (pyvers:infile:[]) <- getArgs
  pypath <- getEnv "PYTHONPATH"
  let env = DG.Environment pyvers (splitSearchPath pypath)
  results <- runReaderT (DG.startGraph infile) env
  result <- DG.displayGraph results
  putStrLn result
