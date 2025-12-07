module Day03 (solve, parseInput) where

import Text.Megaparsec (parse, errorBundlePretty, some, sepBy)
import Text.Megaparsec.Char (digitChar, eol)
import Utils.Parsers (Parser)
import Data.Function.Memoize (memoize2)

type Bank = [Int]
type Input = [Bank]

charsToInts :: [Char] -> [Int]
charsToInts cs = map (\c -> read [c]) cs

parseInput :: Parser Input
parseInput = do
  lns <- (some digitChar) `sepBy` eol
  return $ map charsToInts lns

-- Remove the last element in the list.
dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : (dropLast xs)

highestVoltage :: Bank -> Int
highestVoltage bank = let
  a = maximum (dropLast bank)
  b = maximum $ tail $ dropWhile (/= a) bank
  in (a * 10) + b

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let voltages = map highestVoltage input
  print $ sum voltages

toNumber :: [Int] -> Int
toNumber digits = read (concatMap show digits)

maxBank :: Bank -> Bank -> Bank
maxBank a [] = a
maxBank [] b = b
maxBank a b 
  | length a > length b = a
  | length b > length a = b
  | otherwise = max a b

highestVoltage2 :: Bank -> Int
highestVoltage2 bank = toNumber $ helper bank 12
  where
    helper = memoize2 $ \b n -> case (b, n :: Int) of
      ([], _) -> []
      (_, 0)  -> []
      ((x:xs), _) -> let
        ifIncluded = x : helper xs (n - 1)
        ifExcluded = helper xs n
        in maxBank ifIncluded ifExcluded

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let voltages = map highestVoltage2 input
  print $ sum voltages

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
