module Year2023.Day21 (solve) where

import Data.Function.Memoize
import qualified Data.Set as Set
import Maze
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2023/input/sample/Day21.txt"
-- >>> let Right parsedExample = parse parseInput "" example
-- >>> partA 6 parsedExample
-- 16

type Grid = Maze Char

type Input = Grid

type Points = Set.Set Point

parseInput :: Parser Input
parseInput = do
  ls <- many (char '.' <|> char '#' <|> char 'S') `sepEndBy` newline
  return $ mazeFromList ls

startPoint :: Grid -> Point
startPoint m = head $ filter (testPoint m (== 'S')) (allPoints m)

stepPoint :: Grid -> Point -> Points
stepPoint maze point = memoize (stepHelper maze) point
  where
    stepHelper m p = Set.fromList $ filter (testPoint m (/= '#')) (neighbors4 m p)

step :: Grid -> Points -> Points
step m ps = Set.unions $ Set.map (stepPoint m) ps

partA :: Int -> Input -> Int
partA steps grid =
  let start = startPoint grid
      possibilities = iterate (step grid) (Set.singleton start)
   in length $ possibilities !! steps

part2 :: Input -> IO ()
part2 grid = do
  putStr "Part 2: "
  let start = startPoint grid
  let possibilities = iterate (step grid) (Set.singleton start)
  print $ length $ possibilities !! 26501365

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA 64 input
      part2 input