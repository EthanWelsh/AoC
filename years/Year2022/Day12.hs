{-# LANGUAGE PackageImports #-}
module Year2022.Day12 (solve) where

import           Algorithm.Search
import           Data.Char
import qualified "unordered-containers" Data.HashSet as HashSet
import           Data.List
import           Data.Maybe
import           Matrix
import           Parsers                    (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Maze = (Matrix Char)
type Input = Maze

inputParser :: Parser Input
inputParser = some (noneOf "\n") `sepEndBy` eol

allowedStep :: Maze -> Point -> Point -> Bool
allowedStep maze currP nextP = let
    curr = getNormalized maze currP
    next = getNormalized maze nextP
    in (nextChar curr) >= next

neighborPoints :: Maze -> Point -> HashSet.HashSet Point
neighborPoints maze p = HashSet.filter (allowedStep maze p) (cardinalPoints maze p)

findMatching :: Maze -> (Point -> Bool) -> Point
findMatching maze p = fromJust $ find p (getAllPoints maze)

startPosition :: Maze -> Point
startPosition maze = findMatching maze (\p -> 'S' == (get maze p))

endPosition :: Maze -> Point
endPosition maze = findMatching maze (\p -> 'E' == (get maze p))

nextChar :: Char -> Char
nextChar 'z' = 'z'
nextChar c = chr (ord c + 1)

getNormalized :: Maze -> Point -> Char
getNormalized maze p = case (get maze p) of
    'S' -> 'a'
    'E' -> 'z'
    c -> c

heuristicDistanceToGoal :: Maze -> Point -> Int
heuristicDistanceToGoal maze (r, c) = let
    (r', c') = endPosition maze
    in abs (r - r') + abs (c - c')

isGoal :: Maze -> Point -> Bool
isGoal maze p = p == (endPosition maze)

distanceBetweenNeighbors :: Point -> Point -> Int
distanceBetweenNeighbors _ _ = 1

search :: Maze -> Maybe (Int, [Point])
search maze = aStar (HashSet.toList . neighborPoints maze) distanceBetweenNeighbors (heuristicDistanceToGoal maze) (isGoal maze) (startPosition maze)

partA :: Input -> String
partA maze = show $ fst $ fromJust $ search maze

allStartPoints :: Maze -> [Point]
allStartPoints maze = filter (\p -> 'a' == (getNormalized maze p)) (getAllPoints maze)

searchFromMultiplePoints :: Maze -> [Point] -> Maybe (Int, [Point])
searchFromMultiplePoints maze starts = let
    -- aStar only supports searching from a single start point, but we can easily add
    -- that capability using a fake start point, and marking all real start points as
    -- neighbors of that point.
    fakeStartPoint = (-123, -123)
    neighborOrStartPoint point = if (point == fakeStartPoint) then starts else (HashSet.toList (neighborPoints maze point))
    in aStar neighborOrStartPoint distanceBetweenNeighbors (heuristicDistanceToGoal maze) (isGoal maze) fakeStartPoint

partB :: Input -> String
partB maze = let
    shortestPath = searchFromMultiplePoints maze (allStartPoints maze)
    in show $ fst (fromJust $ shortestPath) - 1

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      putStrLn $ partA input
      putStr "Part 2: "
      putStrLn $ partB input