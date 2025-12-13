{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day11 (solve) where

import Text.Megaparsec (parse, errorBundlePretty, sepEndBy, noneOf, some, sepBy)
import Utils.Parsers (Parser, skipSpaces)
import Utils.Graph
import Control.Monad (void)
import Text.Megaparsec.Char (string, newline)
import Data.Function.Memoize (memoize)

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
      else sum [ go neighbor | neighbor <- neighbors g n ]

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ countPathsBetween input "you" "out"

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let path1 = countPathsBetween input "svr" "dac" * 
              countPathsBetween input "dac" "fft" * 
              countPathsBetween input "fft" "out"
  let path2 = countPathsBetween input "svr" "fft" * 
              countPathsBetween input "fft" "dac" * 
              countPathsBetween input "dac" "out"
  print (path1 + path2)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
