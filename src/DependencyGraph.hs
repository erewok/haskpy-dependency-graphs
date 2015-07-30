module DependencyGraph (
  module Graphs
  , module Imports
  , module Modules
  , startGraph
  , printableNode
  , printGraph
  ) where

import Control.Applicative
import Prelude

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
  putStrLn $ concatMap node nods
