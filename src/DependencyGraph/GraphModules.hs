module DependencyGraph.GraphModules (
  Node(..)
  , makeEdges
  , makeNode
  , discoverAllNodes
  , visitAllNodes
  , generateGraph
  , stop
  , undiscoveredNodes
  , allNodesVisited
  , unvisitedNodes
  , markVisited
  , nodeVisited
  , visited
  , nodeDiscovered
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Prelude
import qualified DependencyGraph.Modules as M


data Node = Node { node :: FilePath
                 , nodes :: [FilePath]
                 , edges :: [(FilePath, FilePath)]
                 } deriving (Show, Eq)

-- Simple strategy:
-- "Discovered" nodes are added to [Node] as Node discoveredPath [] []
-- Visited nodes are then marked Node discoveredPath ["$" ...] [...]
-- where ... may be an empty list or a list of FilePaths

nodeDiscovered :: [Node] -> FilePath -> Bool
nodeDiscovered nodeset fp = fp `elem` (map node nodeset)

undiscoveredNodes :: [Node] -> [FilePath]
undiscoveredNodes nds = nub $ filter todiscover $ concatMap nodes nds
                        where todiscover = \fp -> ((not . nodeDiscovered nds) fp)
                                                   && (fp /= "$")

makeDiscoveredNode :: FilePath -> Node
makeDiscoveredNode fp = Node fp [] []

discoverAllNodes :: [Node] -> [Node]
discoverAllNodes allnodes = allnodes ++ newnodes
                            where newnodes = map makeDiscoveredNode $ undiscoveredNodes allnodes

markVisited :: Node -> Node
markVisited nd
  | nodeVisited nd = nd
  | otherwise = Node infile visitedpaths nodeedges
                 where infile = node nd
                       visitedpaths = "$" : nodes nd
                       nodeedges = edges nd

visited :: [FilePath] -> Bool
visited [] = False
visited ("$":_) = True
visited _ = False

nodeVisited :: Node -> Bool
nodeVisited = visited . nodes

allNodesVisited :: [Node] -> Bool
allNodesVisited = and . map nodeVisited

unvisitedNodes :: [Node] -> [Node]
unvisitedNodes = filter (not . nodeVisited)

visitAllNodes :: M.Environment -> IO [Node] -> IO [Node]
visitAllNodes env nds = do
  allnodes <- nds
  let visitednodes = filter nodeVisited allnodes
  let mustvisit = map node $ unvisitedNodes allnodes
  newlyvisited <- sequence $ (makeNode env) <$> (map return mustvisit)
  return $ visitednodes ++ newlyvisited

makeEdges :: Functor f => FilePath -> f FilePath -> f (FilePath, FilePath)
makeEdges infile fps = (,) infile <$> fps

makeNode :: M.Environment -> IO FilePath -> IO Node
makeNode env infile = do
  file <- infile
  absfile <- M.absolutize file
  paths <- nub <$> M.findAllModules env file
  let nodeEdges = makeEdges absfile paths
  return $ markVisited $ Node absfile paths nodeEdges

stop :: [Node] -> Bool
stop nds = allNodesVisited nds && (null $ undiscoveredNodes nds)

generateGraph :: M.Environment -> IO [Node] -> IO [Node]
generateGraph env nds = do
  continue <- stop <$> nds
  case continue of
    True -> nds
    False -> generateGraph env $ visitAllNodes env (discoverAllNodes <$> nds)
