-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2023/input/sample/Day24.txt"
-- >>> case parse parseInput "" example of Right _ -> True; Left _ -> False
-- True

module Year2023.Day24 (solve) where

import Control.Monad (void)
import Parsers
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)

commaAndSpaces :: Parser ()
commaAndSpaces = do
  void $ char ','
  void $ many (char ' ')

newtype Position = Position (Int, Int, Int) deriving (Show, Eq, Ord)

newtype Velocity = Velocity (Int, Int, Int) deriving (Show, Eq, Ord)

data Hail = Hail Position Velocity deriving (Show, Eq, Ord)

type Input = [Hail]

parseXyz :: Parser (Int, Int, Int)
parseXyz = do
  x <- signedInteger
  commaAndSpaces
  y <- signedInteger
  commaAndSpaces
  z <- signedInteger
  return (x, y, z)

parseLine :: Parser Hail
parseLine = do
  p <- parseXyz
  manySpaces
  void $ string "@"
  manySpaces
  v <- parseXyz
  return (Hail (Position p) (Velocity v))

parseInput :: Parser Input
parseInput = many parseLine

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print input

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
