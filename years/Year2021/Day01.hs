module Year2021.Day01 (solve) where

import           Data.List
import           Parsers                    (Parser, integer)
import           Text.Megaparsec
import           Text.Megaparsec.Char       (eol)

type Input = [Int]

inputParser :: Parser Input
inputParser = integer `sepEndBy` eol

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [x] = []
pairs (x:y:ys) = (x, y) : pairs (y : ys)

countIncreases :: [(Int, Int)] -> Int
countIncreases pairs = length $ filter (\(a, b) -> b > a) pairs

partA :: Input -> String
partA input = show $ countIncreases $ pairs input

sum' :: (Int, Int, Int) -> Int
sum' (a, b, c) = a + b + c

triples :: [Int] -> [(Int, Int, Int)]
triples [] = []
triples [x] = []
triples [x, y] = []
triples (x:y:z:zs) = (x, y, z) : triples (y:z:zs)

partB :: Input -> String
partB input = show $ countIncreases $ pairs (map sum' (triples input))

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