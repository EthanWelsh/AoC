module Year2022.Day04 (solve) where

import Control.Monad (void)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Range = (Int, Int)

type Pair = (Range, Range)

type Input = [Pair]

rangeParser :: Parser Range
rangeParser = do
  a <- L.decimal
  void $ char '-'
  b <- L.decimal
  return (a, b)

pairParser :: Parser Pair
pairParser = do
  a <- rangeParser
  void $ char ','
  b <- rangeParser
  return (a, b)

inputParser :: Parser Input
inputParser = pairParser `sepEndBy` eol

rangeToList :: Range -> [Int]
rangeToList (a, b) = [a .. b]

fullyContains :: Pair -> Bool
fullyContains (a, b) =
  let al = rangeToList a
      bl = rangeToList b
      isSubsetOf needle haystack = all (`elem` haystack) needle
   in isSubsetOf al bl || isSubsetOf bl al

partA :: Input -> Int
partA input = length $ filter (fullyContains) input

overlaps :: Pair -> Bool
overlaps (a, b) =
  let al = rangeToList a
      bl = rangeToList b
      containsAny needle haystack = any (`elem` haystack) needle
   in containsAny al bl

partB :: Input -> Int
partB input = length $ filter (overlaps) input

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
