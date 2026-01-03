{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Year2025.Day11 (solve) where

import Control.Monad (void)
import Data.Function.Memoize (memoize)
import Graph
import Parsers (Parser, skipSpaces)
import Text.Megaparsec (errorBundlePretty, noneOf, parse, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (newline, string)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> let example1 = "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out"
-- >>> let Right parsedExample1 = parse parseInput "" example1
-- >>> let example2 = "svr: aaa bbb\naaa: fft\nfft: ccc\nbbb: tty\ntty: ccc\nccc: ddd eee\nddd: hub\nhub: fff\neee: dac\ndac: fff\nfff: ggg hhh\nggg: out\nhhh: out"
-- >>> let Right parsedExample2 = parse parseInput "" example2

type Input = Graph String

parseLine :: Parser (String, [String])
parseLine = do
  node <- some (noneOf " :\n")
  void $ string ":"
  skipSpaces
  ns <- some (noneOf " \n") `sepBy` string " "
  return (node, ns)

parseInput :: Parser Input
parseInput = do
  ls <- parseLine `sepEndBy` newline
  return $ graphFromList ls

countPathsBetween :: Input -> String -> String -> Int
countPathsBetween g start end = go start
  where
    go = memoize $ \n ->
      if n == end
        then 1
        else sum [go neighbor | neighbor <- neighbors g n]

-- |
-- >>> part1 parsedExample1
-- Part 1: 5
part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ countPathsBetween input "you" "out"

countPathsBetween4 :: Input -> String -> String -> String -> String -> Int
countPathsBetween4 g a b c d =
  countPathsBetween g a b
    * countPathsBetween g b c
    * countPathsBetween g c d

-- |
-- >>> part2 parsedExample2
-- Part 2: 2
part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let path1 = countPathsBetween4 input "svr" "dac" "fft" "out"
  let path2 = countPathsBetween4 input "svr" "fft" "dac" "out"
  print (path1 + path2)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
