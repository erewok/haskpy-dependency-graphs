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


printableNode :: Node -> String
printableNode nd = (++) intro $ unlines $ map combineTp $ edges nd
  where combineTp = (\tp -> "(" ++ fst tp ++ ", " ++ snd tp ++ ") ")
        intro = (node nd) ++ " Edges: "

printGraph :: IO [Node] -> IO ()
printGraph nds = do
  nods <- nds
  putStrLn $ concat $ mapM printableNode nods
  putStrLn "Modules discovered: "
  putStrLn $ concat $ mapM node nods
