module Year2025.Day01 (solve, parseInput, Instruction(..)) where

import           Text.Megaparsec (parse, errorBundlePretty, optional, some, choice)
import           Text.Megaparsec.Char (char, eol)
import Parsers (Parser, integer)
import List (count)


data Instruction = L Int | R Int deriving (Show, Eq)
type Input = [Instruction]

instructionP :: Parser Instruction
instructionP = do
  dir <- choice [char 'L', char 'R']
  n <- integer
  return $ if dir == 'L' then L n else R n

parseInput :: Parser Input
parseInput = some (instructionP <* optional eol)

values :: Input -> [Int]
values ins = scanl update 50 ins
  where
    update acc (L n) = (acc - n) `mod` 100
    update acc (R n) = (acc + n) `mod` 100

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ count (==0) (values input)


explode :: [Instruction] -> [Instruction]
explode [] = []
explode (x:xs) = case x of
  L n -> replicate n (L 1) ++ explode xs
  R n -> replicate n (R 1) ++ explode xs

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print $ count (==0) (values (explode input))

  --print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
