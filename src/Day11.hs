{-# OPTIONS_GHC -Wno-unused-matches #-}
module Day11 (solve) where

import Text.Megaparsec
import Utils.Parsers (Parser, skipSpaces)
import Utils.Graph
import Control.Monad (void)
import Text.Megaparsec.Char (string, newline)

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

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ length $ allPaths input "you" "out"

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let paths = allPaths input "svr" "out"
  let validPaths = filter (\p -> "dac" `elem` p && "fft" `elem` p) paths
  print $ length validPaths

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
