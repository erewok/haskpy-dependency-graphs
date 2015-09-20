module DependencyGraph.Loaders (
  filter3rdPartyStdLibPaths
  , pyFile
  , findPackage
  , findModule
  , findPyObject
  , locateModule
  , locateModules
  , whichPath
  , makeInits
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
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

data PyModule = Pymodule { pymodule :: FilePath,
                           modpath :: FilePath,
                           inits :: [FilePath]
                         } deriving (Show)


isPrefixPath :: String -> String -> String
isPrefixPath x y = if isPrefixOf y x then x else ""

sortByLen :: [String] -> [String]
sortByLen [] = []
sortByLen (x:xs) = largerLengths ++ [x] ++ smallerLengths
                   where
                         largerLengths = sortByLen [g | g <- xs, not $ smaller g x]
                         smallerLengths = sortByLen [h | h <- xs, smaller h x]
                         smaller j y = length j < length y

whichPath :: [FilePath] -> FilePath -> Maybe FilePath
whichPath [] _ = Nothing
whichPath xs y
  | null sortedPrefixes = Nothing
  | otherwise = Just $ head sortedPrefixes
  where sortedPrefixes = sortByLen $ filter (/="") $ map (isPrefixPath y) xs

notPath :: FilePath -> FilePath -> Bool
notPath path = (/=) (dropTrailingPathSeparator path)

-- We don't want our original mdpath so drop 1
middleDirs :: FilePath -> FilePath -> [FilePath]
middleDirs path mdpath = takeWhile (notPath path) (drop 1 $ iterate takeDirectory mdpath)

addInit :: FilePath -> FilePath
addInit fp = joinPath $ (addTrailingPathSeparator fp) : ["__init__.py"]

-- make all inits between a PythonPath dir and the module's location
makeInits :: FilePath -> FilePath -> [FilePath]
makeInits path mdpath = (addInit path) : (map addInit $ middleDirs path (dropTrailingPathSeparator mdpath))

-- This must be implemented somewhere, right?
dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM p whole@(x:xs) = do
  res <- p x
  case res of
    True -> dropWhileM p xs
    False -> return whole

-- Reduce PythonPath to reduce search space
-- Takes a python version and removes anything matching lib/{python vers}
filter3rdPartyStdLibPaths :: String -> [FilePath] -> [FilePath]
filter3rdPartyStdLibPaths pyv = filter nozip . filter noLib
                                where nozip = (not . isSuffixOf "zip")
                                      noLib = (not . isInfixOf ("lib/" ++ pyv))

-- Add "py" extension to FilePath
pyFile :: FilePath -> FilePath
pyFile xs = addExtension (dropTrailingPathSeparator xs) "py"

-- Functions for finding directories
splitDirs :: FilePath -> [FilePath]
splitDirs [] = []
splitDirs fp = if cantSplit fp then [] else takeDirectory fp : (splitDirs $ takeDirectory fp)
  where cantSplit = (\fp' -> takeDirectory fp' == "." || takeDirectory fp' == fp' || null fp')

directoryDoesntExist :: FilePath -> IO Bool
directoryDoesntExist = liftM not . doesDirectoryExist

findFirstMatchingDir :: FilePath -> IO (Maybe FilePath)
findFirstMatchingDir fp = do
  matches <- dropWhileM directoryDoesntExist $ (fp : splitDirs fp)
  case (null matches) of
    True -> return Nothing
    False -> return $ Just $ head matches

-- Cartesian product of PythonPath dirs and one import directory's path.
findPackage :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findPackage _ [] = return Nothing
findPackage pythonpath packpath = do
  let packdir = dropExtension packpath
  let prepped_paths = map addTrailingPathSeparator pythonpath
  let combinations = (++) <$> prepped_paths <*> [packdir]
  result <- catMaybes <$> mapM findFirstMatchingDir combinations
  case (null result) of
    True -> return Nothing
    False -> do
      let package_init = addInit $ head result
      bool <- doesFileExist package_init
      if bool then return $ Just package_init else return Nothing

-- Cartesian product of PythonPath dirs and one import file's path.
-- Return first matching, actual, existing file
findModule :: [FilePath] -> FilePath -> IO (Maybe FilePath)
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
-- findPyObject [] _ = return Nothing
findPyObject _ [] = return Nothing
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
          newpath <- findPackage pythonpath importpath
          return newpath

locateModules :: [FilePath] -> [FilePath] -> IO [Maybe FilePath]
locateModules fp1 fp2 = sequence $ locateModule fp1 <$> fp2
