module Year2025.Day02 (solve) where

import Data.List.Split (chunksOf)
import Parsers (Parser, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> let example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
-- >>> let Right parsedExample = parse parseInput "" example

type Input = [Range]

type Range = (Int, Int)

parseInput :: Parser Input
parseInput = parseRange `sepBy` (char ',')

parseRange :: Parser Range
parseRange = do
  open <- integer
  _ <- char '-'
  close <- integer
  return (open, close)

numsInRange :: Range -> [Int]
numsInRange (start, close) = [start .. close]

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt (length xs `div` 2) xs

isInvalid :: Int -> Bool
isInvalid x =
  let n = show x
      (a, b) = splitInHalf n
   in a == b

allNums :: [Range] -> [Int]
allNums rs = concatMap numsInRange rs

-- |
-- >>> part1 parsedExample
-- Part 1: 1227775554
part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let invalidNums = filter isInvalid (allNums input)
  print $ sum invalidNums

-- [1234] --> [["12", "34"], ["1", "2", "3", "4"]]
allEqualSizedChunks :: [a] -> [[[a]]]
allEqualSizedChunks xs =
  let n = length xs
      possibleChunkSizes = filter (\x -> n `mod` x == 0) [1 .. n `div` 2]
   in map (\c -> chunksOf c xs) possibleChunkSizes

chunkInvalid :: (Eq a) => [[a]] -> Bool
chunkInvalid chunks = all (== head chunks) (tail chunks)

isInvalid2 :: Int -> Bool
isInvalid2 x =
  let n = show x
      possibleChunkSizes = allEqualSizedChunks n
   in any chunkInvalid possibleChunkSizes

-- |
-- >>> part2 parsedExample
-- Part 2: 4174379265
part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let invalidNums = filter isInvalid2 (allNums input)
  print $ sum invalidNums

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
