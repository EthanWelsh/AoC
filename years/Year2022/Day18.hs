module Year2022.Day18 (solve) where

import           Algorithm.Search
import           Control.Monad              (void)
import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Data.Set                   as Set
import           Parsers                    (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Point = (Int, Int, Int)
type Input = [Point]

data Direction = North | South | East | West | Up | Down deriving (Eq, Show)

pointParser :: Parser Point
pointParser = do
    x <- L.decimal
    void $ char ','
    y <- L.decimal
    void $ char ','
    z <- L.decimal
    return (x, y, z)

inputParser :: Parser Input
inputParser = pointParser `sepEndBy` eol

movePoint :: Point -> Direction -> Point
movePoint (x, y, z) North = (x,     y + 1, z)
movePoint (x, y, z) South = (x,     y - 1, z)
movePoint (x, y, z) East =  (x + 1, y,     z)
movePoint (x, y, z) West =  (x - 1, y,     z)
movePoint (x, y, z) Up =    (x,     y,     z + 1)
movePoint (x, y, z) Down =  (x,     y,     z - 1)

openFacesFromPoint :: Set.Set Point -> Point -> [Point]
openFacesFromPoint points point = let
    faces = map (movePoint point) [North, South, East, West, Up, Down]
    in filter (not . (`Set.member` points)) faces

openFaces :: Set.Set Point -> [Point]
openFaces points = concatMap (openFacesFromPoint points) (Set.toList points)

partA :: Input -> String
partA input = show $ length $ openFaces (Set.fromList input)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

neighbors :: Set.Set Point -> Point -> [Point]
neighbors rock point = let
    points = map (movePoint point) [North, South, East, West, Up, Down]
    in filter (not . (`Set.member` rock)) points

reachableFromOutside :: Set.Set Point -> Point -> Bool
reachableFromOutside rock p = let
    goal = (0, 0, 0)
    in isJust $ aStar (neighbors rock) (\_ _ -> 1) (manhattanDistance goal) (==goal) p

partB :: Input -> String
partB input = let
    rock = Set.fromList input
    open = openFaces rock
    in show $ length $ filter (reachableFromOutside rock) open

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