-- Modules does not handle looking up modules
-- inside __init__.py, which is a pretty key feature
-- of Python. As a result, it is likely to perform
-- unreliably for the moment for any project that uses __init__.py
-- to define application imports.

module DependencyGraph.Modules (
  Environment(..)
  , getImports
  , absolutize
  , getPath
  , dotsToPath
  , initialImportPaths
  , findAllModules
  , getPyVers
  , getPyPath
  , EnvT
  ) where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath

import qualified DependencyGraph.ImportLine as I
import qualified DependencyGraph.Loaders as L
import qualified Text.ParserCombinators.Parsec as P


data Environment = Environment { pyvers :: String,
                                 pythonpath :: [FilePath]
                               } deriving (Show)

type EnvT e = ReaderT Environment (IO) e


getPyVers :: EnvT String
getPyVers = do
  pythonvers <- asks pyvers
  return pythonvers

getPyPath :: EnvT [FilePath]
getPyPath = do
  pypaths <- asks pythonpath
  return pypaths


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
  let results = P.parse I.imports "" <$> lines file
  return $ filter cleanResults $ concat $ getGoodResults <$> results

joinWithSeparator :: [String] -> FilePath
joinWithSeparator xs = joinPath $ addTrailingPathSeparator <$> xs

dotsToPath :: FilePath -> FilePath
dotsToPath xs
  | (read xs :: Int) == 1 = "."
  | otherwise = joinWithSeparator $ replicate n dots
                where n = (read xs :: Int) - 1
                      dots = ".."

getPath :: I.Importer -> String
getPath (I.ImportModule xs) = joinWithSeparator xs
getPath (I.RelativeImport (x:xs)) = joinWithSeparator ys
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
    True -> fmap (L.pyFile . getPath) <$>  getImports fname
    False -> return []

-- Functions for testing existance of paths
dropFinal :: FilePath -> FilePath
dropFinal "" = ""
-- dropFinal xs = (L.pyFile . dropTrailingPathSeparator . takeDirectory) xs
dropFinal xs = L.pyFile dropped
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
relAbsPaths :: [FilePath] -> ([FilePath], [FilePath])
relAbsPaths = partition (\fp -> head fp == '.')

-- USING readerT Environment
-- File MUST exist
findAllModules' :: FilePath -> EnvT [FilePath]
findAllModules' pyfile = do
  initial_imports <- initialImportPaths <$> pure pyfile
  (rel_paths, abs_paths) <- lift (liftM relAbsPaths initial_imports)
  version <- getPyVers
  ppath <- getPyPath
  let python_path = L.filter3rdPartyStdLibPaths version ppath
  let dirname = takeDirectory pyfile
  modules <- catMaybes <$> lift (L.locateModules python_path abs_paths)

  initialdir <- lift getCurrentDirectory
  -- absoluteAllRels needs to be in the proper dir to locate rel paths
  lift $ setCurrentDirectory dirname
  other_modules <- getRealPaths <$> lift (absoluteAllRels rel_paths)
  lift $ setCurrentDirectory initialdir
  (++) modules <$> lift other_modules

findAllModules :: FilePath -> EnvT [FilePath]
findAllModules pyfile = do
  lift (doesFileExist pyfile) >>= (\res ->
                                     case res of
                                       True ->findAllModules' pyfile
                                       False -> return [])
