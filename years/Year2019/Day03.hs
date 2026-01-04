module Year2019.Day03 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Maze (Direction (..), Point, manhattanDistance, movePoint)
import Parsers (Parser, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example1 = unsafePerformIO $ readFile "years/Year2019/input/sample/Day03_1.txt"
-- >>> let Right parsedExample1 = parse parseInput "" example1
-- >>> let example2 = unsafePerformIO $ readFile "years/Year2019/input/sample/Day03_2.txt"
-- >>> let Right parsedExample2 = parse parseInput "" example2
-- >>> let example3 = unsafePerformIO $ readFile "years/Year2019/input/sample/Day03_3.txt"
-- >>> let Right parsedExample3 = parse parseInput "" example3

type Step = (Direction, Int)

type Line = [Step]

type Input = (Line, Line)

parseStep :: Parser Step
parseStep = do
  d <-
    choice
      [ North <$ char 'U',
        East <$ char 'R',
        South <$ char 'D',
        West <$ char 'L'
      ]
  v <- integer
  return (d, v)

parseLine :: Parser Line
parseLine = parseStep `sepBy` char ','

parseInput :: Parser Input
parseInput = do
  a <- parseLine
  b <- parseLine
  return (a, b)

step :: Point -> Step -> [Point]
step _ (_, 0) = []
step p (d, c) =
  let newPoint = movePoint p d
      futurePoints = step newPoint (d, c - 1)
   in newPoint : futurePoints

pointsInLine :: Point -> Line -> [Point]
pointsInLine current [] = [current]
pointsInLine current (s : ss) =
  let pointsFromCurrentSegment = step current s
      lastPointInSegment = last pointsFromCurrentSegment
   in pointsFromCurrentSegment ++ pointsInLine lastPointInSegment ss

getCrossingPoints :: [Point] -> [Point] -> [Point]
getCrossingPoints a b = S.toList $ S.intersection (S.fromList a) (S.fromList b)

-- |
-- >>> part1 parsedExample1
-- "6"
-- >>> part1 parsedExample2
-- "159"
-- >>> part1 parsedExample3
-- "135"
part1 :: Input -> String
part1 (a, b) = do
  let origin = (0, 0)
  let aPoints = pointsInLine origin a
  let bPoints = pointsInLine origin b
  let crossingPoints = getCrossingPoints aPoints bPoints
  let distances = map (manhattanDistance origin) crossingPoints
  show $ minimum distances

distanceToPoint :: [Point] -> Int -> M.Map Point Int
distanceToPoint [] _ = M.empty
distanceToPoint (p : ps) d = M.union (M.singleton p d) (distanceToPoint ps (d + 1))

-- |
-- >>> part2 parsedExample1
-- "30"
-- >>> part2 parsedExample2
-- "610"
-- >>> part2 parsedExample3
-- "410"
part2 :: Input -> String
part2 (a, b) = do
  let origin = (0, 0)
  let aPoints = pointsInLine origin a
  let bPoints = pointsInLine origin b
  let aDistances = distanceToPoint aPoints 1
  let bDistances = distanceToPoint bPoints 1
  let crossingPoints = getCrossingPoints aPoints bPoints
  let distances = map (\p -> aDistances M.! p + bDistances M.! p) crossingPoints
  show $ minimum distances

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      putStrLn $ part1 input
      putStr "Part 2: "
      putStrLn $ part2 input
