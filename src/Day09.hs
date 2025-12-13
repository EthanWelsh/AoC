{-# LANGUAGE BlockArguments #-}
module Day09 (solve) where

import Text.Megaparsec
import           Utils.Parsers (Parser)
import Text.Megaparsec.Char (char, eol)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (tails, sortBy)

type Point = (Int, Int)
type Input = [Point]

parsePoint :: Parser Point
parsePoint = (,) <$> L.decimal <* char ',' <*> L.decimal

parseInput :: Parser Input
parseInput = parsePoint `sepEndBy` eol

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let pairs = allPairs input
  let sorted = sortBy (flip \(a,b) (c,d) -> compare (area a b) (area c d)) pairs
  let topArea = uncurry area (head sorted)
  print topArea
  --print $ area $ head sorted

part2 :: Input -> IO ()
part2 _ = do
  putStr "Part 2: "

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
