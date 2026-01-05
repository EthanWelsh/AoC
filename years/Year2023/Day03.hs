module Year2023.Day03 (solve) where

import Data.Char (isDigit)
import Data.List (nub)
import Maze

-- $setup
-- >>> import Maze (mazeFromList)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = mazeFromList . lines $ unsafePerformIO $ readFile "years/Year2023/input/sample/Day03.txt"
-- >>> partA example
-- 4361
-- >>> partB example
-- 467835

type Board = Maze Char

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && (c /= '.')

hasAdjacentSymbol :: Board -> Point -> Bool
hasAdjacentSymbol board point =
  let ns = neighbors8 board point
      cs = map (getPoint board) ns
   in any isSymbol cs

findLeftStart :: Board -> Point -> Point
findLeftStart board point = if isPointStart then point else findLeftStart board l
  where
    l = movePoint point West
    isOutOfBounds = not (inBounds board l)
    hasNoMoreDigits = not $ isDigit (getPoint board l)
    isPointStart = isOutOfBounds || hasNoMoreDigits

allDigitPoints :: Board -> [Point]
allDigitPoints board = filter (testPoint board isDigit) (allPoints board)

allPointsToRight :: Board -> Point -> [Point]
allPointsToRight board (r, c) = [(r, x) | x <- [c .. (width board - 1)]]

getNumberFromOrigin :: Board -> Point -> Int
getNumberFromOrigin board point =
  let numPoints = takeWhile (testPoint board isDigit) (allPointsToRight board point)
   in read (map (getPoint board) numPoints)

getNumbersAroundPoint :: Board -> Point -> [Int]
getNumbersAroundPoint board point =
  let ns = neighbors8 board point
      numPoints = filter (testPoint board isDigit) ns
      origins = nub $ map (findLeftStart board) numPoints
   in map (getNumberFromOrigin board) origins

gearRatio :: Board -> Point -> Int
gearRatio board point = case getNumbersAroundPoint board point of
  [a, b] -> a * b
  _ -> 0

partA :: Board -> Int
partA board =
  let digitPoints = allDigitPoints board
      nextToSymbols = filter (hasAdjacentSymbol board) digitPoints
      origins = nub $ map (findLeftStart board) nextToSymbols
      nums = map (getNumberFromOrigin board) origins
   in sum nums

partB :: Board -> Int
partB board =
  let gearPoints = filter (testPoint board (== '*')) (allPoints board)
      gearRatios = map (gearRatio board) gearPoints
   in sum gearRatios

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  let board = mazeFromList $ lines contents
  putStrLn $ "Part 1:" ++ show (partA board)
  putStrLn $ "Part 2:" ++ show (partB board)