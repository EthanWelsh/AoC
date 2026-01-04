module Year2019.Day04 (solve) where

import Control.Monad (void)
import Data.List (group)
import Parsers (Parser, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

type Input = (Int, Int)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : y : ys) = (x, y) : pairs (y : ys)

-- |
-- >>> isSixDigits 111111
-- True
-- >>> isSixDigits 223450
-- True
-- >>> isSixDigits 123789
-- True
-- >>> isSixDigits 12345
-- False
isSixDigits :: Int -> Bool
isSixDigits n = 6 == length (show n)

-- |
-- >>> hasTwoSameAdjacentDigits 111111
-- True
-- >>> hasTwoSameAdjacentDigits 223450
-- True
-- >>> hasTwoSameAdjacentDigits 123789
-- False
hasTwoSameAdjacentDigits :: Int -> Bool
hasTwoSameAdjacentDigits n = any (uncurry (==)) ((pairs . show) n)

-- |
-- >>> digitsNeverDecrease 111111
-- True
-- >>> digitsNeverDecrease 223450
-- False
-- >>> digitsNeverDecrease 123789
-- True
digitsNeverDecrease :: Int -> Bool
digitsNeverDecrease n = not $ any (uncurry (>)) ((pairs . show) n)

parseInput :: Parser Input
parseInput = do
  a <- integer
  void $ char '-'
  b <- integer
  return (a, b)

allp :: [a -> Bool] -> a -> Bool
allp ps a = and (map ($ a) ps)

part1 :: Input -> String
part1 (a, b) = do
  let predicates = [isSixDigits, hasTwoSameAdjacentDigits, digitsNeverDecrease]
  let possibles = [a .. b]
  let filtered = filter (allp predicates) possibles
  show $ length filtered

-- |
-- >>> noLargeGroups 112233
-- True
-- >>> noLargeGroups 123444
-- False
-- >>> noLargeGroups 111122
-- True
noLargeGroups :: Int -> Bool
noLargeGroups n = any ((== 2) . length) (group (show n))

part2 :: Input -> String
part2 (a, b) = do
  let predicates = [isSixDigits, hasTwoSameAdjacentDigits, digitsNeverDecrease, noLargeGroups]
  let possibles = [a .. b]
  let filtered = filter (allp predicates) possibles
  show $ length filtered

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
