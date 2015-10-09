module DependencyGraph.GraphModules (
  Node(..)
  , makeEdges
  , makeNode
  , makeNode'
  , discoverAllNodes
  , generateGraph
  , generateGraph'
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
import Control.Monad.Trans (lift)
import Data.List
import qualified DependencyGraph.Loaders as L
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
nodeDiscovered nodeset fp = fp `elem` map node nodeset

undiscoveredNodes :: [Node] -> [FilePath]
undiscoveredNodes nds = nub $ filter todiscover $ concatMap nodes nds
                        where todiscover fp = (not . nodeDiscovered nds) fp
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
allNodesVisited = all nodeVisited

unvisitedNodes :: [Node] -> [Node]
unvisitedNodes = filter (not . nodeVisited)

makeEdges :: Functor f => FilePath -> f FilePath -> f (FilePath, FilePath)
makeEdges infile fps = (,) infile <$> fps

stop :: [Node] -> Bool
stop nds = allNodesVisited nds && null (undiscoveredNodes nds)


-- USING readerT Environment
--
makeNode :: IO FilePath -> M.EnvT Node
makeNode infile = do
  file <- lift infile
  absfile <- lift (M.absolutize file)
  simple_paths <- nub <$> M.findAllModules file
  let nodeEdges = makeEdges absfile simple_paths
  return $ markVisited $ Node absfile simple_paths nodeEdges

visitAllNodes :: M.EnvT [Node] -> M.EnvT [Node]
visitAllNodes nds = do
  allnodes <- nds
  let visitednodes = filter nodeVisited allnodes
  let mustvisit = node <$> unvisitedNodes allnodes
  let newlyvisited = makeNode <$> map return mustvisit
  (++) <$> return visitednodes <*> sequence newlyvisited

generateGraph :: M.EnvT [Node] -> M.EnvT [Node]
generateGraph nds = do
  continue <- stop <$> nds
  case continue of
    True -> nds
    False -> generateGraph $ visitAllNodes (discoverAllNodes <$> nds)


makeNode' :: IO FilePath -> M.EnvT Node
makeNode' infile = do
  file <- lift infile
  absfile <- lift (M.absolutize file)
  simple_paths <-  M.findAllModules'' file
  let pymodule_paths = (++) (nub $ concatMap L.pyinits simple_paths)
                            (nub $ map L.pymodule simple_paths)
  let nodeEdges = makeEdges absfile pymodule_paths
  return $ markVisited $ Node absfile pymodule_paths nodeEdges

visitAllNodes' :: M.EnvT [Node] -> M.EnvT [Node]
visitAllNodes' nds = do
  allnodes <- nds
  let visitednodes = filter nodeVisited allnodes
  let mustvisit = node <$> unvisitedNodes allnodes
  let newlyvisited = makeNode' <$> map return mustvisit
  (++) <$> return visitednodes <*> sequence newlyvisited

generateGraph' :: M.EnvT [Node] -> M.EnvT [Node]
generateGraph' nds = do
  continue <- stop <$> nds
  case continue of
    True -> nds
    False -> generateGraph' $ visitAllNodes' (discoverAllNodes <$> nds)
