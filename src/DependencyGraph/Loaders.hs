module DependencyGraph.Loaders (
  filter3rdPartyStdLibPaths
  , pyFile
  , PythonPaths
  , PyModule(..)
  , findPackage
  , findModule
  , findPyObject
  , locateModule
  , locateModules
  , locateModules'
  , makeInits
  , makeModule
  , middleDirs
  , whichPath
  , findPrefixPath
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath


data PyModule = Pymodule { pymodule :: FilePath,
                           modpypath :: FilePath,
                           pyinits :: [FilePath]
                         }

type PythonPaths = [FilePath]


sortByLen :: [String] -> [String]
sortByLen [] = []
sortByLen (x:xs) = largerLengths ++ [x] ++ smallerLengths
                   where
                         largerLengths = sortByLen [g | g <- xs, not $ smaller g x]
                         smallerLengths = sortByLen [h | h <- xs, smaller h x]
                         smaller j y = length j < length y


-- Could use pypath from reader env
findPrefixPath :: PythonPaths -> FilePath -> FilePath
findPrefixPath paths fp = let
    sortedPaths = sortByLen paths
    prefixTesters = isPrefixOf <$> sortedPaths
    results = map ($ fp) prefixTesters
    pathPositions = zip results [1..] :: [(Bool, Int)]
    matches = filter fst pathPositions
    result = if (not . null) matches then (snd . head) matches else -1
    foundPath = if result >= 0 && result < length paths then paths !! result else ""
  in foundPath

-- Could use pypath from reader env
whichPath :: PythonPaths -> FilePath -> Maybe FilePath
whichPath [] _ = Nothing
whichPath paths fp = if foundPath == "" then Nothing else Just foundPath
  where foundPath = findPrefixPath paths fp

notPath :: FilePath -> FilePath -> Bool
notPath path = (/=) (dropTrailingPathSeparator path)

-- We don't want our original mdpath so drop 1
middleDirs :: FilePath -> FilePath -> [FilePath]
middleDirs path mdpath = takeWhile (notPath path)
                         (drop 1 $ iterate takeDirectory mdpath)

addInit :: FilePath -> FilePath
addInit fp = joinPath $ (addTrailingPathSeparator fp) : ["__init__.py"]

-- make all inits between a PythonPath dir and the module's location
makeInits :: FilePath -> FilePath -> [FilePath]
makeInits path mdpath = (:) (addInit path)
                        (map addInit $ middleDirs path (dropTrailingPathSeparator mdpath))

makeModule :: PythonPaths -> FilePath -> IO (Maybe PyModule)
makeModule ppath fp = do
  let solePath = whichPath ppath fp
  case solePath of
    Nothing -> return Nothing
    Just path -> return $ Just Pymodule {pymodule = fp,
                                         modpypath = path,
                                         pyinits = makeInits path fp}


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
filter3rdPartyStdLibPaths :: String -> PythonPaths -> PythonPaths
filter3rdPartyStdLibPaths pyv = filter nozip . filter noLib
                                where nozip = not . isSuffixOf "zip"
                                      noLib = not . isInfixOf ("lib/" ++ pyv)

-- Add "py" extension to FilePath
pyFile :: FilePath -> FilePath
pyFile xs = addExtension (dropTrailingPathSeparator xs) "py"

-- Functions for finding directories
splitDirs :: FilePath -> [FilePath]
splitDirs [] = []
splitDirs fp = if cantSplit fp then [] else takeDirectory fp : splitDirs (takeDirectory fp)
  where cantSplit fp' = takeDirectory fp' == "." || takeDirectory fp' == fp' || null fp'

directoryDoesntExist :: FilePath -> IO Bool
directoryDoesntExist = liftM not . doesDirectoryExist

findFirstMatchingDir :: FilePath -> IO (Maybe FilePath)
findFirstMatchingDir fp = do
  matches <- dropWhileM directoryDoesntExist (fp : splitDirs fp)
  case (null matches) of
    True -> return Nothing
    False -> return $ Just $ head matches


-- Could use pypath from reader env
-- Cartesian product of PythonPath dirs and one import directory's path.
findPackage :: PythonPaths -> FilePath -> IO (Maybe FilePath)
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

-- Could use pypath from reader env
-- Cartesian product of PythonPath dirs and one import file's path.
-- Return first matching, actual, existing file
findModule :: PythonPaths -> FilePath -> IO (Maybe FilePath)
findModule _ [] = return Nothing
findModule _ ".py" = return Nothing
findModule pythonpath modpath = do
  let prepped_paths = map addTrailingPathSeparator pythonpath
  let combinations = (++) <$> prepped_paths <*> [modpath]
  result <- filterM doesFileExist combinations
  case (null result) of
    True -> return Nothing
    False -> return $ Just $ head result

-- Could use pypath from reader env
-- Return FilePath (module) that exists and is part of object's FilePath
-- Does not look for existence of Object inside module
findPyObject :: PythonPaths -> FilePath -> IO (Maybe FilePath)
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
      findModule pythonpath shortened

-- Could use pypath from reader env
locateModule :: PythonPaths -> FilePath -> IO (Maybe FilePath)
locateModule [] _ = return Nothing
locateModule xs [] = return $ Just $ head xs
locateModule pythonpath importpath = do
  isModule <- findModule pythonpath importpath
  case isModule of
    (Just pmodule) -> return $ Just pmodule
    Nothing -> do
      isPyObject <- findPyObject pythonpath importpath
      case isPyObject of
        (Just pyObject) -> return $ Just pyObject
        Nothing -> findPackage pythonpath importpath

locateModules :: PythonPaths -> [FilePath] -> IO [Maybe FilePath]
locateModules fp1 fp2 = sequence $ locateModule fp1 <$> fp2

locateModules' :: PythonPaths -> [FilePath] -> IO [PyModule]
locateModules' fp1 fp2 = do
  maybePaths <- sequence $ locateModule fp1 <$> fp2
  let maybeModules = catMaybes maybePaths
  result <- fmap (makeModule fp1) <$> pure maybeModules
  catMaybes <$> sequence result
