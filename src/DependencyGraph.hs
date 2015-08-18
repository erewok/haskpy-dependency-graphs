module DependencyGraph (
  module Graphs
  , module Imports
  , module Modules
  , module Loaders
  , startGraph
  , printableNode
  , printGraph
  , displayGraph
  ) where

import Control.Applicative
import Data.String.Utils
import System.IO
import Paths_dependency_graph

import DependencyGraph.GraphModules as Graphs
import DependencyGraph.ImportLine as Imports
import DependencyGraph.Modules as Modules
import DependencyGraph.Loaders as Loaders

startGraph :: Environment -> FilePath -> IO [Node]
startGraph env infile = do
  firstNode <- makeNode env (pure infile)
  generateGraph env (pure [firstNode])


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

displayGraph :: IO [Node] -> IO ()
displayGraph nods = do
  template <- getDataFileName "html/index.html"
  content <- readFile template
  let all_edges = concat <$> filter (not . null) <$> map edges <$> nods
  links <- concatMap edgeToLink <$> all_edges
  let index = replace sub links content
  putStrLn index
