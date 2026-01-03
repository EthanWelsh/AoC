module Year2022.Day01 (solve) where

import           Data.List (maximum, map, sum, filter)
import qualified Data.List as L
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Parsers (Parser)

type Input = [[Int]]

inputParser :: Parser Input
inputParser = (L.decimal `sepEndBy` eol) `sepEndBy` (eol >> eol)

partA :: Input -> Int
partA input = maximum $ map sum input

qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = let
  larger = qs $ filter (>x) xs
  equal = x : filter (==x) xs
  smaller = qs $ filter (<x) xs
  in larger ++ equal ++ smaller

partB :: Input -> Int
partB input = let
    sortedTotals = qs (map sum input)
    in sum $ L.take 3 sortedTotals

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
