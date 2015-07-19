-- Modules does not handle looking up modules
-- inside __init__.py, which is a pretty key feature
-- of Python. As a result, it is likely to perform
-- unreliably for the moment for any project that uses __init__.py
-- to define application imports.

module DependencyGraph.Modules (
  Environment(..),
  getImports,
  absolutize,
  getPath,
  dotsToPath,
  initialImportPaths,
  getRealPath,
  getRealPaths,
  locateModule,
  locateModules,
  filter3rdPartyStdLibPaths,
  findAllModules
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
--import Control.Monad.Reader
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.List
import Data.List.Split
import Data.Maybe
import Prelude
import System.Directory
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)

import qualified DependencyGraph.ImportLine as I
import qualified Text.ParserCombinators.Parsec as P


data Environment = Environment { pyvers :: String,
                                 pythonpath :: [FilePath]
                               } deriving (Show)
-- type Env r = ReaderT Environment (IO) r

-- -- test reader env
-- testreader = Environment "python3.4" testpythonpath


-- Tried and failed to put PythonPath and Python version into ReaderT: ToDo
-- getPyVers :: Env String
-- getPyVers = asks pyvers
--   -- env <- ask
--   -- return $ pyvers env

-- getPyPath :: Env [FilePath]
-- getPyPath = asks pythonpath
--             -- do
--   -- env <- ask
--   -- return $ pythonpath env


-- FP Complete Provided the following
absolutize :: FilePath -> IO FilePath
absolutize aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

absoluteAllRels :: [FilePath] -> IO [FilePath]
absoluteAllRels = sequence . map absolutize


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
initialImportPaths :: FilePath -> IO [FilePath]
initialImportPaths fname = do
  res <- doesFileExist fname
  case res of
    True -> fmap (pyFile . getPath) <$>  getImports fname
    False -> return []

-- Functions for testing existance of paths
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

-- Partition into relative and absolute paths
-- List must not be empty: FilePaths must not be empty
relAbsPaths :: [FilePath] -> ([FilePath], [FilePath])
relAbsPaths = partition (\fp -> head fp == '.')


--                                     --
-- Functions that deal with PythonPath --
--                                     --

-- Take a PythonPath and a list of 'import' FilePaths and figures out which are real paths
-- Strategy: for each import path, take the first element and run a cartesian product with all possible
-- paths from PythonPath. Then filter against doesDirectoryExist and return first matching

-- Cartesian product of PythonPath dirs and one import file's path. Chop off first part
locateModule :: [FilePath] -> FilePath -> IO (Maybe FilePath)
locateModule [] _ = return Nothing  -- empty PythonPath: cannot locate
locateModule _ [] = return Nothing  -- empty FilePath: does not exist
locateModule xs y = do
  let splitted = splitOn "/" y
  let moddir = head splitted
  let rest = intercalate "/" $ tail $ splitted
  location <- locateModuleDir $ (++) <$> xs <*> [moddir]
  return $ (++) <$> location <*> Just rest

-- Look for one module's directory in list: return Maybe FilePath for first one that exists, Nothing if None
locateModuleDir :: [FilePath] -> IO (Maybe FilePath)
locateModuleDir xs = do
  res <- filterM doesDirectoryExist xs
  getDirPath res

-- Take directory Paths and match first directory that exists
getDirPath :: [FilePath] -> IO (Maybe FilePath)
getDirPath fps
  | length fps == 0 = return Nothing
  | otherwise = return $ Just (head fps)

-- Takes a list of PythonPaths and a list of imprted Paths and returns actual paths
locateModules :: [FilePath] -> [FilePath] -> IO [Maybe FilePath]
locateModules fp1 fp2 = sequence $ locateModule fp1 <$> fp2

-- Takes a python version and removes anything matching lib/{python vers}
filter3rdPartyStdLibPaths :: String -> [FilePath] -> [FilePath]
filter3rdPartyStdLibPaths pyv xs = filter (\fp -> not $ isInfixOf ("lib/" ++ pyv) fp) xs


-- Plan:
-- Get import modules: initialImports filename
-- Separate into "relative" and "other paths": relAbsPaths filepaths
-- Filter 3rd Party and Stdlib out of PythonPaths: filter3rd... pyvers pythonpaths
-- Combine "other paths" with paths from PythonPath: locateModules PythonPaths FilePaths
-- validate paths: getRealPaths filepaths

findAllModules :: Environment -> FilePath -> IO [FilePath]
findAllModules env pyfile = do
  initial_imports <- initialImportPaths <$> return pyfile
  (rel_paths, abs_paths) <- liftM relAbsPaths initial_imports
  let version = pyvers env
  let ppath = pythonpath env
  let python_path = filter3rdPartyStdLibPaths version ppath
  modules <- catMaybes <$> locateModules python_path abs_paths
  other_modules <- getRealPaths <$> absoluteAllRels rel_paths
  (++) modules <$> other_modules
