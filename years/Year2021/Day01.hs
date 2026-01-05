module Year2021.Day01 (solve) where

import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (eol)
import qualified Text.Megaparsec.Char.Lexer as L

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2021/input/sample/Day01.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- "7"
-- >>> partB parsedExample
-- "5"

type Input = [Int]

integer' :: Parser Int
integer' = L.decimal

inputParser :: Parser Input
inputParser = integer' `sepEndBy` eol

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [_] = []
pairs (x : y : ys) = (x, y) : pairs (y : ys)

countIncreases :: [(Int, Int)] -> Int
countIncreases ps = length $ filter (\(a, b) -> b > a) ps

partA :: Input -> String
partA input = show $ countIncreases $ pairs input

sum' :: (Int, Int, Int) -> Int
sum' (a, b, c) = a + b + c

triples :: [Int] -> [(Int, Int, Int)]
triples [] = []
triples [_] = []
triples [_, _] = []
triples (x : y : z : zs) = (x, y, z) : triples (y : z : zs)

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