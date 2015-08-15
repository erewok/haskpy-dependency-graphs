module DependencyGraph.Loaders (
  filter3rdPartyStdLibPaths
  , pyFile
  , findPackage
  , findModule
  , findPyObject
  , locateModule
  , locateModules
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Prelude
import System.Directory
import System.FilePath

---------
-- ANALYSIS --

-- Cases
-- Package exists: parse init
-- module exists: parse init of parents and package
-- class exists inside module: find module and do module
-- class exists inside package: find package and do package
---------


-- One of these imports may be one of three different things
-- This is unused for the moment, but perhaps can help with locateModule at bottom
data PyImport = PyModule FilePath
              | PyPackage FilePath
              | PyObject FilePath

-- Reduce PythonPath to reduce search space
-- Takes a python version and removes anything matching lib/{python vers}
filter3rdPartyStdLibPaths :: String -> [FilePath] -> [FilePath]
filter3rdPartyStdLibPaths pyv = filter nozip . filter noLib
                                where nozip = (not . isSuffixOf "zip")
                                      noLib = (not . isInfixOf ("lib/" ++ pyv))

-- Add "py" extension to FilePath
pyFile :: FilePath -> FilePath
pyFile xs = addExtension (dropTrailingPathSeparator xs) "py"

-- Cartesian product of PythonPath dirs and one import directory's path.
findPackage :: [FilePath] -> FilePath -> IO ([FilePath], FilePath)
findPackage xs [] = return (xs, "")
findPackage [] _ = return ([], "")
findPackage pythonpath packpath = do
  let (x:xs) = splitPath $ dropExtension packpath
  let remainder = joinPath xs
  let prepped_paths = map addTrailingPathSeparator pythonpath
  let combinations = (++) <$> prepped_paths <*> [x]
  result <- filterM doesDirectoryExist combinations
  case (null result) of
    True -> return ([], packpath)
    False -> return (result, (addExtension "py" remainder))

-- Cartesian product of PythonPath dirs and one import file's path.
-- Return first matching, actual, existing file
findModule :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findModule [] _ = return Nothing
findModule _ [] = return Nothing
findModule _ ".py" = return Nothing
findModule pythonpath modpath = do
  let prepped_paths = map addTrailingPathSeparator pythonpath
  let combinations = (++) <$> prepped_paths <*> [modpath]
  result <- filterM doesFileExist combinations
  case (null result) of
    True -> return Nothing
    False -> return $ Just $ result !! 0

-- Return FilePath (module) that exists and is part of object's FilePath
-- Does not look for existence of Object inside module
findPyObject :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findPyObject [] _ = return Nothing
findPyObject pythonpath modpath
  | (length $ splitPath modpath) > 1 = do
      let shortened = pyFile $ (joinPath . init . splitPath) modpath
      isfound <- findModule pythonpath shortened
      case isfound of
        Nothing -> findPyObject pythonpath shortened
        (Just a) -> return $ Just a
  | otherwise = do
      let shortened = pyFile $ (joinPath . init . splitPath) modpath
      found <- findModule pythonpath shortened
      return found

locateModule :: [FilePath] -> FilePath -> IO (Maybe FilePath)
locateModule [] _ = return Nothing
locateModule xs [] = return $ Just $ xs !! 0
locateModule pythonpath importpath = do
  isModule <- findModule pythonpath importpath
  case isModule of
    (Just pymodule) -> return $ Just pymodule
    Nothing -> do
      isPyObject <- findPyObject pythonpath importpath
      case isPyObject of
        (Just pyObject) -> return $ Just pyObject
        Nothing -> do
          (newpath, remainder) <- findPackage pythonpath importpath
          case (null newpath) of
            True -> return Nothing
            False -> locateModule newpath remainder

locateModules :: [FilePath] -> [FilePath] -> IO [Maybe FilePath]
locateModules fp1 fp2 = sequence $ locateModule fp1 <$> fp2
