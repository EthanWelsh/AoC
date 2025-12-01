module Utils.Maze (
  Point,
  Maze,
  Direction(North, South, East, West),
  movePoint,
  mazeFromList,
  mazeToList,
  height,
  width,
  getPoint,
  setPoint,
  setPoints,
  setPointsMatching,
  findPoints,
  testPoint,
  inBounds,
  neighbors4,
  neighbors8,
  allPoints,
  allPointsSatisfying,
  manhattanDistance,
) where

import           Control.Lens

type Point = (Int, Int)
newtype Maze a = Maze [[a]] deriving (Eq, Ord)
data Direction = North | South | East | West deriving (Show, Eq, Ord)

instance Show a => Show (Maze a) where
  show (Maze m) = unlines $ map (concatMap show) m

-- | Construct a 'Maze' from a list-of-rows representation.
mazeFromList :: [[a]] -> Maze a
mazeFromList = Maze

-- | Convert a 'Maze' back into a list-of-rows representation.
mazeToList :: Maze a -> [[a]]
mazeToList (Maze m) = m

north :: Point -> Point
north (r, c) = (r - 1, c)

east :: Point -> Point
east (r, c) = (r, c + 1)

south :: Point -> Point
south (r, c) = (r + 1, c)

west :: Point -> Point
west (r, c) = (r, c - 1)

northEast :: Point -> Point
northEast = north . east

southEast :: Point -> Point
southEast = south . east

southWest :: Point -> Point
southWest = south . west

northWest :: Point -> Point
northWest = north . west

movePoint :: Point -> Direction -> Point
movePoint p North = north p
movePoint p East  = east p
movePoint p South = south p
movePoint p West  = west p

-- | Get the height (number of rows) of the maze.
height :: Maze a -> Int
height (Maze m) = length m

-- | Get the width (number of columns) of the maze.
width :: Maze a -> Int
width (Maze m) = length $ head m

-- | Read the value at a given point (row, column).
getPoint :: Maze a -> Point -> a
getPoint (Maze m) (r, c) = (m !! r) !! c

replacePoint :: [[a]] -> Point -> a -> [[a]]
replacePoint g (r, c) v = let
    oldRow = g !! r
    newRow = (element c .~ v) oldRow
  in (element r .~ newRow) g

-- | Set the value at a single point in the maze, returning a new maze.
setPoint :: Maze a -> Point -> a -> Maze a
setPoint (Maze m) p v = Maze (replacePoint m p v)

-- | Set the same value at multiple points in the maze.
setPoints :: Maze a -> [Point] -> a -> Maze a
setPoints m ps v = foldl (\mm p -> setPoint mm p v) m ps

-- | Set a value at every point that matches a predicate.
setPointsMatching :: Maze a -> (a -> Bool) -> a -> Maze a
setPointsMatching m f v = let
  ps = filter (testPoint m f) (allPoints m)
  in foldl (\mm p -> setPoint mm p v) m ps

-- | Test whether the value at a point satisfies a predicate.
testPoint :: Maze a -> (a -> Bool) -> Point -> Bool
testPoint m f p = f (getPoint m p)

-- | Find all points whose values satisfy a predicate.
findPoints :: Maze a -> (a -> Bool) -> [Point]
findPoints m f = filter (testPoint m f) (allPoints m)

-- | Check whether a point is within the bounds of the maze.
inBounds :: Maze a -> Point -> Bool
inBounds m (r, c) = r >= 0 && r < height m && c >= 0 && c < width m

-- | List the 4 orthogonal neighbors of a point (that are in-bounds).
neighbors4 :: Maze a -> Point -> [Point]
neighbors4 m p = filter (inBounds m) $ map ($ p) dirs
  where dirs = [north, east, south, west]

-- | List the 8 surrounding neighbors of a point (that are in-bounds).
neighbors8 :: Maze a -> Point -> [Point]
neighbors8 m p = filter (inBounds m) $ map ($ p) dirs
  where dirs = [north, northEast, east, southEast, south, southWest, west, northWest]

-- | List every point in the maze in row-major order.
allPoints :: Maze a -> [Point]
allPoints m = [(r, c) | r <- [0 .. (height m - 1)], c <- [0 .. (width m - 1)]]

-- | Filter all points by a predicate on points.
allPointsSatisfying :: Maze a -> (Point -> Bool) -> [Point]
allPointsSatisfying m f = filter f (allPoints m)

-- | Compute the Manhattan (L1) distance between two points.
manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
