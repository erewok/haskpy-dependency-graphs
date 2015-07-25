module DependencyGraph.GraphModules (
  Node(..)
  , makeEdges
  , makeNode
  , discoverAllNodes
  , visitAllNodes
  , generateGraph
  ) where

import Control.Applicative
import Control.Monad
import Prelude
import qualified DependencyGraph.Modules as M


data Node = Node { node :: FilePath
                 , nodes :: [FilePath]
                 , edges :: [(FilePath, FilePath)]}

makeEdges :: Functor f => FilePath -> f FilePath -> f (FilePath, FilePath)
makeEdges infile fps = (,) infile <$> fps

makeNode :: M.Environment -> IO FilePath -> IO Node
makeNode env infile = do
  file <- infile
  absfile <- M.absolutize file
  paths <- M.findAllModules env file
  let nodeEdges = makeEdges absfile paths
  return $ Node absfile paths nodeEdges

-- Simple strategy:
-- "Discovered" nodes are added to [Node] as Node discoveredPath [] []
-- Visited nodes are then marked Node discoveredPath ["$" ...] [...]
-- where ... may be an empty list or a list of FilePaths

nodeDiscovered :: [Node] -> FilePath -> Bool
nodeDiscovered nodeset fp = fp `elem` (map node nodeset)

undiscoveredNodes :: [Node] -> [FilePath]
undiscoveredNodes nds = filter (nodeDiscovered nds) $ concatMap nodes nds

makeDiscoveredNode :: FilePath -> Node
makeDiscoveredNode fp = Node fp [] []

discoverAllNodes :: [Node] -> [Node]
discoverAllNodes allnodes = (++) allnodes $ map makeDiscoveredNode $ undiscoveredNodes allnodes

visited :: [FilePath] -> Bool
visited [] = False
visited ("$":_) = True
visited _ = False

nodeVisited :: Node -> Bool
nodeVisited = visited . nodes

markVisited :: Node -> Node
markVisited nd
  | nodeVisited nd = nd
  | otherwise = Node infile visitedpaths nodeedges
                 where infile = node nd
                       visitedpaths = "$" : nodes nd
                       nodeedges = edges nd

unvisitedNodes :: [Node] -> [Node]
unvisitedNodes = filter nodeVisited

visitAllNodes :: M.Environment -> IO [Node] -> IO [Node]
visitAllNodes env nds = do
  allnodes <- nds
  let mustvisit = unvisitedNodes allnodes
  let tovisit = pure <$> node <$> mustvisit
  let newlyvisited = sequence $ liftM markVisited . makeNode env <$> tovisit
  (++) <$> nds <*> newlyvisited

allNodesVisited :: [Node] -> Bool
allNodesVisited = and . map nodeVisited

stop :: [Node] -> Bool
stop nds = allNodesVisited nds && (null $ undiscoveredNodes nds)

generateGraph :: M.Environment -> IO [Node] -> IO [Node]
generateGraph env nds = do
  continue <- liftM stop nds
  case continue of
    True -> nds
    False -> visitAllNodes env nds
