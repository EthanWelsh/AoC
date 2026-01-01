module Year2022.Day08 (solve) where

import           Data.List
import           Matrix
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Parsers (Parser)

type Trees = Matrix Char
type Input = Trees

inputParser :: Parser Input
inputParser = some (noneOf "\n") `sepEndBy` eol

range :: Int -> Int -> [Int]
range start end
    | start < end  = [start..end]
    | start > end  = [start, start - 1 .. end]
    | start == end = []

getLinesOfSite :: Trees -> Point -> [[Point]]
getLinesOfSite ts (r, c) = let
    height = (getHeight ts) - 1
    width = (getWidth ts) - 1
    up = zip    (range r 0)      (repeat c)
    down = zip  (range r height) (repeat c)
    left = zip  (repeat r)       (range c 0)
    right = zip (repeat r)       (range c width)
    in [up, down, left, right]

isStrictlyDecreasing :: Ord a => [a] -> Bool
isStrictlyDecreasing [] = True
isStrictlyDecreasing (x:xs) = all (<x) xs

isVisible :: Trees -> Point -> Bool
isVisible ts p = let
    linesOfSitePoints = getLinesOfSite ts p                   :: [[Point]]
    linesOfSiteValues = map (map (get ts)) linesOfSitePoints  :: [[Char]]
    isDecreasing = map isStrictlyDecreasing linesOfSiteValues :: [Bool]
    in or isDecreasing

countWhere :: (a -> Bool) -> [a] -> Int
countWhere pred xs = length $ filter pred xs

partA :: Input -> Int
partA trees = let
    allPoints = getAllPoints trees
    in countWhere (isVisible trees) allPoints

countDecreasingFrom :: Ord a => a -> [a] -> Int
countDecreasingFrom _ [] = 0
countDecreasingFrom t (x:xs) = if x >= t then 1 else 1 + countDecreasingFrom t xs

countDecreasing :: Ord a => [a] -> Int
countDecreasing [] = 0
countDecreasing (x:xs) = countDecreasingFrom x xs

getScore :: Trees -> Point -> Int
getScore ts p = let
    linesOfSitePoints = getLinesOfSite ts p                   :: [[Point]]
    linesOfSiteValues = map (map (get ts)) linesOfSitePoints  :: [[Char]]
    in product $ map countDecreasing linesOfSiteValues

partB :: Input -> Int
partB ts = let
    allPoints = getAllPoints ts :: [Point]
    allScores = map (getScore ts) allPoints :: [Int]
    in maximum allScores

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: "
      print $ partB input