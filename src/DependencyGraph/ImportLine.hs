{-# LANGUAGE OverloadedStrings #-}
-- What is an import line?
-- it's a line of spaces, text, and periods
-- it might also have a continuation '\' + '\n' or  '(' + '\n'
-- importLine styles:
-- import x  ✓
-- import x.y  ✓
-- from x import y ✓
-- from x.y import z  ✓
-- import x as y ✓
-- from x.y import z as a ✓
-- from x.y import z, a ✓
-- from .x.y import z as a, c as b, d ✓

-- harder:
-- from x import (\n
--               y,
-- from x import y \ \n
--               z
module DependencyGraph.ImportLine (
  Importer(..),
  imports
  ) where

import Control.Applicative
import Data.Monoid

import qualified Text.ParserCombinators.Parsec as P hiding ((<|>))

data Importer =
  Whitespace
  | ImportModule [String]
  | NameSpace String
  | RelativeImport [String]
  | RelativePath Int
  | Other String
  deriving (Show, Eq)

instance Monoid Importer where
  mempty = Whitespace
  mappend = concatImports

concatImports :: Importer -> Importer -> Importer
concatImports (ImportModule xs) (ImportModule ys) = ImportModule $ xs ++ ys
concatImports (ImportModule xs) (RelativeImport ys) = ImportModule $ xs ++ ys
concatImports (ImportModule xs) (NameSpace y) = ImportModule $ xs ++ [y]
concatImports (RelativePath n) (RelativeImport xs) = RelativeImport $ (show n) : xs
concatImports (RelativeImport xs) (NameSpace y) = RelativeImport $ xs ++ [y]
concatImports (Other xs) (Other ys) = Other $ xs ++ ys
concatImports Whitespace Whitespace = Whitespace

addNamespaces :: Importer -> [Importer] -> [Importer]
addNamespaces _ [] = []
addNamespaces xs (y:ys) = [(concatImports xs y)] ++ (addNamespaces xs ys)

importKeyword :: P.Parser  String
importKeyword = P.string "import"
              <|> P.string "from"

idLetter :: P.Parser Char
idLetter = P.alphaNum <|> P.oneOf "_"

identifier :: P.Parser String
identifier =  P.many1 idLetter

importModule :: P.Parser Importer
importModule = ImportModule <$> identifier `P.sepBy` P.char '.'

nameSpace :: P.Parser Importer
nameSpace = NameSpace <$> identifier

countDots :: P.Parser Importer
countDots = do
  dots <- P.many1 $ P.char '.'
  return (RelativePath $ length dots )

relativeModule :: P.Parser Importer
relativeModule = RelativeImport <$> identifier `P.sepBy` P.char '.'

importAs :: P.Parser Importer
importAs = do
  P.string "as"
  P.space
  ident <- identifier
  return (NameSpace ident)

-- "... as x,g as g"
-- "... as x, g as g"
namespace :: P.Parser Importer
namespace = do
  optional P.space
  ns <- nameSpace
  optional P.space
  optional importAs
  return ns

namespaces :: P.Parser [Importer]
namespaces =  namespace `P.sepBy` P.char ','

simpleImport :: P.Parser [Importer]
simpleImport = do
  importKeyword
  P.spaces
  imodule <- importModule
  optional importAs
  return [imodule]

fromImport :: P.Parser [Importer]
fromImport = do
  importKeyword
  P.spaces
  imodule <- importModule
  P.spaces
  importKeyword
  P.spaces
  ns <- namespaces
  optional importAs
  return (addNamespaces imodule ns)

-- need to keep track of relative path (dots)
relativeImport :: P.Parser [Importer]
relativeImport = do
  importKeyword
  P.spaces
  dots <- countDots
  imodule <- relativeModule
  P.spaces
  importKeyword
  P.spaces
  ns <- namespaces
  optional importAs
  return $ addNamespaces (concatImports dots imodule) ns

fromRelativeImport :: P.Parser [Importer]
fromRelativeImport = P.try relativeImport
                     <|> fromImport

imports :: P.Parser [Importer]
imports = P.try fromRelativeImport
          <|> simpleImport
