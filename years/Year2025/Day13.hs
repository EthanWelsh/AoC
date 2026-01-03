module Year2025.Day13 (solve) where

import Parsers (Parser)
import Text.Megaparsec (parse, errorBundlePretty)

-- 
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2025/input/sample/Day13.txt"
-- >>> let Right parsedExample = parse parseInput "" example

type Input = String

parseInput :: Parser Input
parseInput = error "TODO"

-- |
-- >>> part1 parsedExample
-- Part 1: 0
part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print input

-- |
-- >>> part2 parsedExample
-- Part 2: 0
part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
