module Year2025.Day01 (solve, parseInput, Instruction (..)) where

import List (count)
import Parsers (Parser, integer)
import Text.Megaparsec (choice, errorBundlePretty, optional, parse, some)
import Text.Megaparsec.Char (char, eol)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2025/input/sample/Day01.txt"
-- >>> let Right parsedExample = parse parseInput "" example
-- >>> part1 parsedExample
-- Part 1: 3
-- >>> part2 parsedExample
-- Part 2: 6

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
  let valList = values input
  print $ count (== 0) (tail valList)

explode :: [Instruction] -> [Instruction]
explode [] = []
explode (x : xs) = case x of
  L n -> replicate n (L 1) ++ explode xs
  R n -> replicate n (R 1) ++ explode xs

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print $ count (== 0) (tail (values (explode input)))

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
