module Search
  ( hasPath,
    allPaths,
    bfs,
  )
where

import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- | Determine whether there exists a path from the start node to any node
--   satisfying the 'isEnd' predicate using the provided neighbor function.
hasPath :: (Ord a) => (a -> [a]) -> (a -> Bool) -> a -> Bool
hasPath neighbors isEnd = hasPathHelper neighbors isEnd S.empty

hasPathHelper :: (Ord a) => (a -> [a]) -> (a -> Bool) -> S.Set a -> a -> Bool
hasPathHelper neighbors isEnd visited n
  | isEnd n = True
  | n `S.member` visited = False
  | otherwise =
      let newVisited = S.insert n visited
       in any (hasPathHelper neighbors isEnd newVisited) (neighbors n)

-- | Enumerate all simple paths (no repeated nodes) from the start node to
--   nodes satisfying 'isEnd', returning each path as a list of nodes.
allPaths :: (Ord a) => (a -> [a]) -> (a -> Bool) -> a -> [[a]]
allPaths neighbors isEnd = allPathsHelper neighbors isEnd []

allPathsHelper :: (Ord a) => (a -> [a]) -> (a -> Bool) -> [a] -> a -> [[a]]
allPathsHelper neighbors isEnd path n
  | isEnd n = [path]
  | n `elem` path = []
  | otherwise = concatMap (allPathsHelper neighbors isEnd (n : path)) (neighbors n)

-- | Perform a breadth-first search.
bfs :: (Ord a) => (a -> [a]) -> (a -> Bool) -> a -> Maybe Int
bfs getNeighbors isGoal start = bfs' (Seq.singleton (start, 0)) (S.singleton start)
  where
    bfs' Seq.Empty _ = Nothing
    bfs' ((curr, dist) :<| queue) visited
      | isGoal curr = Just dist
      | otherwise =
          let newNeighbors = filter (`S.notMember` visited) (getNeighbors curr)
              newVisited = foldr S.insert visited newNeighbors
              newQueue = foldr (\n q -> q |> (n, dist + 1)) queue newNeighbors
           in bfs' newQueue newVisited
