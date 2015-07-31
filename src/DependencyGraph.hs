module DependencyGraph (
  module Graphs
  , module Imports
  , module Modules
  , startGraph
  , printableNode
  , printGraph
  , displayGraph
  ) where

import Control.Applicative
import Data.String.Utils
import Prelude
import System.IO

import DependencyGraph.GraphModules as Graphs
import DependencyGraph.ImportLine as Imports
import DependencyGraph.Modules as Modules

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
edgeToLink (a, b) = "{source: \"" ++ a ++ "\", target: \"" ++ b ++ "\", type: \"Direct\"},\n"

displayGraph :: IO [Node] -> IO ()
displayGraph nods = do
  template <- readFile "src/html/index.html"
  let all_edges = concat <$> filter (not . null) <$> map edges <$> nods
  links <- concatMap edgeToLink <$> all_edges
  let index = replace sub links template
  putStrLn index
