{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}

module Graph
  ( Graph,
    edges,
    graphFromList,
    graphFromNodes,
    graphFromEdges,
    graphAsMap,
    graphFromMap,
    apply,
    addEdge,
    addEdges,
    removeEdge,
    removeBidirectionalEdge,
    neighbors,
    nodes,
    reachable,
    connectedComponents,
    makeBidirectional,
    addBidirectionalEdge,
    addBidirectionalEdges,
    allPaths,
  )
where

import Data.List ((\\))
import Data.List.Extra (groupSort)
import Data.Map as M
  (
    Map,
    adjust,
    findWithDefault,
    fromList,
    insertWith,
    keys,
    toList,
  )
import Data.Set
  (
    Set,
    empty,
    insert,
    member,
    notMember,
  )
import Data.Tuple (swap)

-- $setup
-- >>> import qualified Data.Map as M
-- >>> import qualified Data.Set as S
-- >>> import Data.List (sort)
-- >>> let g = graphFromNodes [1, 2, 3]
-- >>> let edgesToAdd = [(1, 2), (2, 3)]
-- >>> let g' = addEdges g edgesToAdd
-- >>> let m = graphAsMap g'
-- >>> let gBi = makeBidirectional g'
-- >>> let mBi = graphAsMap gBi

newtype Graph a = Graph (Map a [a]) deriving (Show)

-- |
-- >>> sort <$> M.lookup 1 mBi
-- Just [2]
-- >>> sort <$> M.lookup 2 mBi
-- Just [1,3]
makeBidirectional :: (Ord a) => Graph a -> Graph a
makeBidirectional g =
  let allEdges = edges g ++ map swap (edges g)
   in graphFromEdges allEdges

-- | Return all edges in the graph as pairs (src, dst).
edges :: Graph a -> [(a, a)]
edges g = concatMap flatten (M.toList (graphAsMap g))

flatten :: (a, [b]) -> [(a, b)]
flatten (a, bs) = map (a,) bs

unflatten :: (Ord a) => [(a, b)] -> [(a, [b])]
unflatten = groupSort

-- | Build a 'Graph' from a list of nodes. Nodes will have no edges.
graphFromNodes :: (Ord a) => [a] -> Graph a
graphFromNodes ns = graphFromMap $ M.fromList $ map (,[]) ns

-- | Build a 'Graph' from a list of adjacency lists.
graphFromList :: (Ord a) => [(a, [a])] -> Graph a
graphFromList lst = graphFromMap $ M.fromList lst

-- | Build a 'Graph' from a list of directed edges.
graphFromEdges :: (Ord a) => [(a, a)] -> Graph a
graphFromEdges es = graphFromMap $ M.fromList $ unflatten es

-- | Apply a transformation to the underlying map representation.
apply :: (Map a [a] -> Map a [a]) -> Graph a -> Graph a
apply f (Graph g) = Graph $ f g

-- | Expose the underlying map representation of the graph.
graphAsMap :: Graph a -> Map a [a]
graphAsMap (Graph m) = m

-- | Construct a 'Graph' directly from a map of adjacency lists.
graphFromMap :: Map a [a] -> Graph a
graphFromMap = Graph

-- | Add a directed edge (src -> dst) to the graph.
addEdge :: (Ord a) => Graph a -> a -> a -> Graph a
addEdge g src dst = apply (M.insertWith (++) src [dst]) g

-- | Add multiple directed edges to the graph.
--
-- >>> M.lookup 1 m
-- Just [2]
-- >>> M.lookup 2 m
-- Just [3]
-- >>> M.lookup 3 m
-- Just []
addEdges :: (Ord a) => Graph a -> [(a, a)] -> Graph a
addEdges = foldr (\(src, dst) g -> addEdge g src dst)

-- | Add bidirectional edge (both directions) to the graph.
addBidirectionalEdge :: (Ord a) => Graph a -> (a, a) -> Graph a
addBidirectionalEdge g (a, b) =
  let g1 = addEdge g a b
      g2 = addEdge g1 b a
   in g2

-- | Add multiple bidirectional edges to the graph.
addBidirectionalEdges :: (Ord a) => Graph a -> [(a, a)] -> Graph a
addBidirectionalEdges = foldr (flip addBidirectionalEdge)

-- | Remove a directed edge (src -> dst) from the graph.
removeEdge :: (Ord a) => Graph a -> (a, a) -> Graph a
removeEdge g (src, dst) = apply (adjust (\es -> es \\ [dst]) src) g

-- | Remove an edge and its reverse (both directions) from the graph.
removeBidirectionalEdge :: (Ord a) => Graph a -> (a, a) -> Graph a
removeBidirectionalEdge g e =
  let g1 = removeEdge g e
      g2 = removeEdge g1 (swap e)
   in g2

-- | Return the adjacency list for a given node.
neighbors :: (Ord a) => Graph a -> a -> [a]
neighbors (Graph g) n = M.findWithDefault [] n g

-- | List all nodes present in the graph.
nodes :: Graph a -> [a]
nodes (Graph g) = keys g

-- | Compute the set of nodes reachable from a start node (including it).
--
-- >>> reachable g' 1
-- fromList [1,2,3]
-- >>> reachable g' 2
-- fromList [2,3]
-- >>> reachable g' 3
-- fromList [3]
reachable :: (Ord a) => Graph a -> a -> Set a
reachable g = helper g empty
  where
    helper gg visited nn
      | nn `member` visited = visited
      | otherwise =
          let newVisited = insert nn visited
              ns = neighbors gg nn
           in foldl (helper gg) newVisited ns

-- | Partition the graph into connected components (as sets of nodes).
connectedComponents :: (Ord a) => Graph a -> [Set a]
connectedComponents g = connectedHelper g (nodes g)
  where
    connectedHelper _ [] = []
    connectedHelper gg (n : ns) =
      let reachableNodes = reachable gg n
          unvisited = filter (`notMember` reachableNodes) ns
       in reachableNodes : connectedHelper gg unvisited

-- | Find all simple paths from start to end nodes.
allPaths :: (Ord a) => Graph a -> a -> a -> [[a]]
allPaths g start end = case start == end of
  True -> [[start]]
  False -> concatMap (\n -> map (start :) (allPaths g n end)) nextNodes
    where
      nextNodes = neighbors g start
