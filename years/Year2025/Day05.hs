module Year2025.Day05 (solve) where

import Control.Monad (void)
import Parsers (Parser)
import Range (Range, Ranges, memberOfRanges, mergeRanges, rangeSize)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as L

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2025/input/sample/Day05.txt"
-- >>> let Right parsedExample = parse parseInput "" example
-- >>> part1 parsedExample
-- Part 1: 3
-- >>> part2 parsedExample
-- Part 2: 14

type Food = [Int]

type Input = (Ranges, Food)

rangeParser :: Parser Range
rangeParser = do
  start <- L.decimal
  _ <- char '-'
  end <- L.decimal
  return (start, end)

parseInput :: Parser Input
parseInput = do
  rs <- rangeParser `sepEndBy` newline
  void newline
  nums <- L.decimal `sepEndBy` newline
  eof
  return (rs, nums)

part1 :: Input -> IO ()
part1 (rs, ns) = do
  putStr "Part 1: "
  print $ length $ filter (memberOfRanges rs) ns

part2 :: Input -> IO ()
part2 (rs, _) = do
  putStr "Part 2: "
  let mergedRanges = mergeRanges rs
  print $ sum $ map rangeSize mergedRanges

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
