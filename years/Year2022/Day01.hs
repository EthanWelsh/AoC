module Year2022.Day01 (solve) where

import Data.List (filter, map, maximum, sum)
import qualified Data.List as List
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2022/input/sample/Day01.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- 24000
-- >>> partB parsedExample
-- 45000

type Input = [[Int]]

inputParser :: Parser Input
inputParser = some (L.decimal <* eol) `sepBy` eol

partA :: Input -> Int
partA input = maximum $ map sum input

qs :: [Int] -> [Int]
qs [] = []
qs (x : xs) =
  let larger = qs $ filter (> x) xs
      equal = x : filter (== x) xs
      smaller = qs $ filter (< x) xs
   in larger ++ equal ++ smaller

partB :: Input -> Int
partB input =
  let sortedTotals = qs (map sum input)
   in sum $ List.take 3 sortedTotals

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
