module DependencyGraph.Modules (
  getImports,
  getPath,
  makeImportPaths
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import Prelude
import System.IO
import System.Directory

import qualified DependencyGraph.ImportLine as I
import qualified Text.ParserCombinators.Parsec as P

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

dotsToPath :: String -> String
dotsToPath xs
  | (read xs :: Int) == 1 = "."
  | (read xs :: Int) == 2 = ".."
  | otherwise = intercalate "/" $ replicate n dots
                where n = (read xs :: Int) - 1
                      dots = ".."

getPath :: I.Importer -> String
getPath (I.ImportModule xs) = intercalate "/" xs
getPath (I.RelativeImport (x:xs)) = intercalate "/" ys
                                  where ys = dots : xs
                                        dots = dotsToPath x

-- This will produce 'supposed' paths for each import
-- However, not all of these will exist, because many of the final
-- elements will represent objects inside the imported module
-- ex: "../some/package/other_test" where "other_test" is really an object inside the "package" module
makeImportPaths :: FilePath -> IO [I.Importer] -> IO [FilePath]
makeImportPaths fname = do
  let result = getImports fname
  return (fmap getPath <$> result)

-- getRealPaths :: IO [FilePath] -> IO [FilePath]
-- getRealPaths = filterM doesFileExist

-- fixedFile :: FilePath -> IO FilePath
-- fixedFile [] = return []
-- fixedFile fp = do
--   fp' <- doesFileExist fp
--   when (not fp')
--     fixedFile $ intercalate "/" $ init $ splitOn "/" fp
--   return fp

-- fixBrokenFiles :: IO [FilePath] -> IO [FilePath]
-- fixBrokenFiles [] = []
-- fixBrokenFiles (x:xs) = fixedFile x : fixBrokenFiles xs


-- Walk directory: http://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
-- check file exists: http://rosettacode.org/wiki/Check_that_file_exists#Haskell
