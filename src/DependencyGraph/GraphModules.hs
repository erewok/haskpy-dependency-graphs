{-# LANGUAGE OverloadedStrings #-}

module DependencyGraph.GraphModules (
  Node(..),
  makeVertices,
  makeNode
  ) where

import Control.Applicative
import Control.Monad
import Prelude
import qualified DependencyGraph.Modules as M


data Node = Node { node :: FilePath
                 , nodes :: [FilePath]
                 , vertices :: [(FilePath, FilePath)]}

makeVertices :: Functor f => FilePath -> f FilePath -> f (FilePath, FilePath)
makeVertices infile fps = (,) infile <$> fps

makeNode :: M.Environment -> IO FilePath -> IO Node
makeNode env infile = do
  file <- infile
  absfile <- M.absolutize file
  paths <- M.findAllModules env file
  let nodeVertices = makeVertices absfile paths
  return $ Node absfile paths nodeVertices

-- processModule :: Node -> [Node] -> [Node]
-- processModule = []

-- graphModules :: M.Environment -> FilePath -> [Node] -> [Node]
-- graphModules env infile [] = []



-- -- Walk directory: http://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
-- -- See: Chapter 18 of Real World Haskell
