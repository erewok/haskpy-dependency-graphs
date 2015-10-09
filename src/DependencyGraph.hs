module DependencyGraph (
  module Graphs
  , module Imports
  , module Modules
  , module Loaders
  , startGraph
  , displayGraph
  , printableNode
  , printGraph
  ) where

import Control.Applicative
import Data.String.Utils
import System.IO
import Paths_dependency_graph

import DependencyGraph.GraphModules as Graphs
import DependencyGraph.ImportLine as Imports
import DependencyGraph.Modules as Modules
import DependencyGraph.Loaders as Loaders


printEdge :: (String, String) -> String
printEdge (a, b) = "(" ++ a ++ ", " ++ b ++ ")\n"

printableNode :: Node -> String
printableNode nd
  | null $ edges nd = ""
  | otherwise = node nd ++ " Edges: \n" ++ concatMap printEdge (edges nd) ++ "\n\n"

printGraph :: IO [Node] -> IO ()
printGraph nds = do
  nods <- nds
  putStrLn $ concatMap printableNode nods
  putStrLn "Modules discovered: "
  putStrLn $ concatMap (\n -> node n ++ "\n") nods

sub :: String
sub = "//### EDGES ###//\n"

edgeToLink :: (String, String) -> String
edgeToLink (a, b) = "{source: \"" ++ a ++ "\", target: \"" ++ b ++ "\", type: \"direct\"},\n"

-- rework to use readerT Environment ...
startGraph :: FilePath -> EnvT [Node]
startGraph infile = do
  firstNode <- makeNode' (pure infile)
  generateGraph' (pure [firstNode])


displayGraph :: [Node] -> IO String
displayGraph nods = do
  template <- getDataFileName "html/index.html"
  content <- readFile template
  let all_edges = (concat . filter (not . null)) $ map edges nods
  let links = concatMap edgeToLink all_edges
  replace sub <$> pure links <*> pure content
