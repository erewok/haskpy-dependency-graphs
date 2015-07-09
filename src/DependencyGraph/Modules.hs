-- Modules does not handle looking up modules
-- inside __init__.py, which is a pretty key feature
-- of Python. As a result, it is likely to perform
-- unreliably for the moment for any project that uses __init__.py
-- to define application imports.

module DependencyGraph.Modules (
  getImports,
  getPath,
  dotsToPath,
  initialImportPaths,
  getRealPath,
  getRealPaths
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.List.Split
import Data.Maybe
import Prelude
import System.Directory

import qualified DependencyGraph.ImportLine as I
import qualified Text.ParserCombinators.Parsec as P

testpythonpath :: [String]
testpythonpath = [""
            , "/opt/boxen/homebrew/Cellar/python3/3.4.2_1/Frameworks/Python.framework/Versions/3.4/lib/python34.zip"
            , "/opt/boxen/homebrew/Cellar/python3/3.4.2_1/Frameworks/Python.framework/Versions/3.4/lib/python3.4"
            , "/opt/boxen/homebrew/Cellar/python3/3.4.2_1/Frameworks/Python.framework/Versions/3.4/lib/python3.4/plat-darwin"
            , "/opt/boxen/homebrew/Cellar/python3/3.4.2_1/Frameworks/Python.framework/Versions/3.4/lib/python3.4/lib-dynload"
            , "/opt/boxen/homebrew/lib/python3.4/site-packages"]


type PythonPath fp = [FilePath]

cleanResults :: I.Importer -> Bool
cleanResults (I.ImportModule []) = False
cleanResults _ = True

getGoodResults :: Either a [I.Importer] -> [I.Importer]
getGoodResults (Left _) = []
getGoodResults (Right []) = []
getGoodResults (Right xs) = xs

getImports :: FilePath -> IO [I.Importer]
getImports fname = do
  file <- readFile fname
  let results = fmap (P.parse I.imports "") $ lines file
  return $ filter cleanResults $ concat $ fmap getGoodResults results

dotsToPath :: FilePath -> FilePath
dotsToPath xs
  | (read xs :: Int) == 1 = "."
  | otherwise = intercalate "/" $ replicate n dots
                where n = (read xs :: Int) - 1
                      dots = ".."

pyFile :: FilePath -> FilePath
pyFile = (++".py")

getPath :: I.Importer -> String
getPath (I.ImportModule xs) = intercalate "/" xs
getPath (I.RelativeImport (x:xs)) = intercalate "/" ys
                                  where ys = dots : xs
                                        dots = dotsToPath x

-- This will produce 'supposed' paths for each import
-- However, not all of these will exist, because many of the final
-- elements will represent objects inside the imported module
-- ex: "../some/package/class" where "class" is really an object inside the "package" module
initialImportPaths :: FilePath -> IO [I.Importer] -> IO [FilePath]
initialImportPaths fname = do
  let result = getImports fname
  return $ fmap (pyFile . getPath) <$> result

dropFinal :: FilePath -> FilePath
dropFinal "" = ""
dropFinal xs = pyFile dropped
               where dropped = intercalate "/" $ init $ splitOn "/" xs

getRealPath :: FilePath -> IO (Maybe FilePath)
getRealPath [] = return Nothing
getRealPath ".py" = return Nothing
getRealPath fp = do
  res <- doesFileExist fp
  case res of
   True -> return $ Just fp
   False -> getRealPath $ dropFinal fp

getRealPaths :: [FilePath] -> IO [FilePath]
getRealPaths = liftM catMaybes . sequence . fmap getRealPath


-- List must not be empty: FilePaths must not be empty
relAbsPaths :: [FilePath] -> ([FilePath], [FilePath])
relAbsPaths = partition (\fp -> head fp == '.')




--normalizePythonPaths :: [FilePath] -> IO [FilePath]
--normalizePythonPaths = liftM filter doesDirectoryExist


-- Other ideas
-- Takes a PythonPath and a list of FilePaths and figures out which are real paths
locateModules :: [FilePath] -> [FilePath] -> [FilePath]
locateModules fp1 fp2 = undefined

-- Removes anything not included in the Strings (packages) passed in
-- "lib/python"
filter3rdPartyStdLib :: [String] -> [FilePath] -> [FilePath]
filter3rdPartyStdLib packages filepaths = undefined



-- Walk directory: http://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
-- check file exists: http://rosettacode.org/wiki/Check_that_file_exists#Haskell
-- See also: Chapter 18 of Real World Haskell
