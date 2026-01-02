module Year2019.Day01 (solve) where

import           Parsers (Parser, integer)
import           Text.Megaparsec
import           Text.Megaparsec.Char (eol)

type Input = [Int]

parseInput :: Parser Input
parseInput = integer `sepEndBy` eol

fuelRequired :: Int -> Int
fuelRequired m = div m 3 - 2

part1 :: Input -> String
part1 input = show $ sum $ map fuelRequired input

fuelRequiredPt2 :: Int -> Int
fuelRequiredPt2 m
  | n <= 0 = 0
  | otherwise = n + fuelRequiredPt2 n
  where
    n = fuelRequired m

part2 :: Input -> String
part2 input = show $ sum $ map fuelRequiredPt2 input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            putStr "Part 1: "
            putStrLn $ part1 input
            putStr "Part 2: "
            putStrLn $ part2 input