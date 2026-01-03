module Year2025.Day06 (solve) where

import Data.Char (isSpace)
import Data.List (transpose, unlines)
import Data.Maybe (mapMaybe)
import List (splitOn)

-- |
-- >>> let example = unlines ["123 328  51 64 ", " 45 64  387 23 ", "  6 98  215 314", "*   +   *   +  "]
-- >>> part1 example
-- Part 1: 4277556
calculate :: ([Int], Char) -> Int
calculate (nums, op) = case op of
  '+' -> sum nums
  '*' -> product nums
  _ -> error "Unknown operator"

parsePart1 :: String -> [([Int], Char)]
parsePart1 contents =
  let allLines = lines contents
      nonEmptyLines = filter (not . null) allLines

      operatorLine = last nonEmptyLines
      numberLines = init nonEmptyLines

      rows = map (map read . words) numberLines
      ops = map head (words operatorLine)

      columns = transpose rows
   in zip columns ops

part1 :: String -> IO ()
part1 contents = do
  putStr "Part 1: "
  let problems = parsePart1 contents
  print $ sum $ map calculate problems

parseCephalopodMath :: String -> [([Int], Char)]
parseCephalopodMath contents =
  let nonEmptyLines = filter (not . null) (lines contents)
      grid = padAndTranspose nonEmptyLines
      isSep = all isSpace
      groups = splitOn isSep grid
   in map parseGroup groups

padAndTranspose :: [String] -> [String]
padAndTranspose lines' =
  let maxLen = maximum (map length lines')
      pad s = s ++ replicate (maxLen - length s) ' '
   in transpose (map pad lines')

parseGroup :: [String] -> ([Int], Char)
parseGroup cols =
  let op = parseOperator cols
      nums = mapMaybe parseColumnNumber cols
   in (nums, op)

parseOperator :: [String] -> Char
parseOperator cols = head $ filter (not . isSpace) (map last cols)

parseColumnNumber :: String -> Maybe Int
parseColumnNumber col =
  let digits = filter (not . isSpace) (init col)
   in if null digits then Nothing else Just (read digits)

-- |
-- >>> let example = unlines ["123 328  51 64 ", " 45 64  387 23 ", "  6 98  215 314", "*   +   *   +  "]
-- >>> part2 example
-- Part 2: 3263827
part2 :: String -> IO ()
part2 contents = do
  putStr "Part 2: "
  let problems = parseCephalopodMath contents
  print $ sum $ map calculate problems

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  part1 contents
  part2 contents
