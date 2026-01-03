module Maze
  ( Point,
    Maze,
    Direction (North, South, East, West),
    north,
    south,
    east,
    west,
    northEast,
    southEast,
    southWest,
    northWest,
    movePoint,
    maybeGetPoint,
    mazeFromList,
    mazeToList,
    mazeFromDimensions,
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
  )
where

import Control.Lens

type Point = (Int, Int)

newtype Maze a = Maze [[a]] deriving (Eq, Ord)

data Direction = North | South | East | West deriving (Show, Eq, Ord)

instance (Show a) => Show (Maze a) where
  show (Maze m) = unlines $ map (concatMap show) m

-- | Construct a 'Maze' from a list-of-rows representation.
mazeFromList :: [[a]] -> Maze a
mazeFromList = Maze

-- | Convert a 'Maze' back into a list-of-rows representation.
mazeToList :: Maze a -> [[a]]
mazeToList (Maze m) = m

mazeFromDimensions :: Int -> Int -> a -> Maze a
mazeFromDimensions w h x = mazeFromList $ replicate h $ replicate w x

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
movePoint p East = east p
movePoint p South = south p
movePoint p West = west p

-- | Get the height (number of rows) of the maze.
height :: Maze a -> Int
height (Maze m) = length m

-- | Get the width (number of columns) of the maze.
width :: Maze a -> Int
width (Maze m) = length $ head m

maybeGetPoint :: Maze a -> Point -> Maybe a
maybeGetPoint (Maze m) p
  | inBounds (Maze m) p = Just $ getPoint (Maze m) p
  | otherwise = Nothing

-- | Read the value at a given point (row, column).
getPoint :: Maze a -> Point -> a
getPoint (Maze m) (r, c)
  | r < 0 || r >= length m = error $ "getPoint: row out of bounds " ++ show (r, c) ++ " height=" ++ show (length m)
  | c < 0 || c >= length (m !! r) = error $ "getPoint: col out of bounds " ++ show (r, c) ++ " rowLen=" ++ show (length (m !! r)) ++ " headWidth=" ++ show (length (head m))
  | otherwise = (m !! r) !! c

replacePoint :: [[a]] -> Point -> a -> [[a]]
replacePoint g (r, c) v =
  let oldRow = g !! r
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
setPointsMatching m f v =
  let ps = filter (testPoint m f) (allPoints m)
   in foldl (\mm p -> setPoint mm p v) m ps

-- | Test whether the value at a point satisfies a predicate.
testPoint :: Maze a -> (a -> Bool) -> Point -> Bool
testPoint m f p = case maybeGetPoint m p of
  Just v -> f v
  Nothing -> False

-- | Find all points whose values satisfy a predicate.
findPoints :: Maze a -> (a -> Bool) -> [Point]
findPoints m f = filter (testPoint m f) (allPoints m)

-- | Check whether a point is within the bounds of the maze.
inBounds :: Maze a -> Point -> Bool
inBounds m@(Maze rows) (r, c) =
  r >= 0 && r < length rows && c >= 0 && c < length (rows !! r)

-- | List the 4 orthogonal neighbors of a point (that are in-bounds).
neighbors4 :: Maze a -> Point -> [Point]
neighbors4 m p = filter (inBounds m) $ map ($ p) dirs
  where
    dirs = [north, east, south, west]

-- | List the 8 surrounding neighbors of a point (that are in-bounds).
neighbors8 :: Maze a -> Point -> [Point]
neighbors8 m p = filter (inBounds m) $ map ($ p) dirs
  where
    dirs = [north, northEast, east, southEast, south, southWest, west, northWest]

-- | List every point in the maze in row-major order.
allPoints :: Maze a -> [Point]
allPoints (Maze rows) = concatMap (\r -> map (\c -> (r, c)) [0 .. (length (rows !! r) - 1)]) [0 .. length rows - 1]

-- | Filter all points by a predicate on points.
allPointsSatisfying :: Maze a -> (Point -> Bool) -> [Point]
allPointsSatisfying m f = filter f (allPoints m)

-- | Compute the Manhattan (L1) distance between two points.
manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
