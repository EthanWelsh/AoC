module Year2022.Day03 (solve) where

import Data.List (length, map, sum, zip)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = [String]

inputParser :: Parser Input
inputParser = some (noneOf "\n") `sepEndBy` eol

splitInHalf :: String -> (String, String)
splitInHalf s = splitAt ((length s) `div` 2) s

inCommon :: (String, String) -> String
inCommon (a, b) = Set.toList $ Set.intersection (Set.fromList a) (Set.fromList b)

score :: Char -> Int
score c =
  let charToScore = Map.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 .. 52]
   in charToScore Map.! c

scoreLine :: String -> Int
scoreLine l = sum $ map score $ inCommon $ splitInHalf l

partA :: Input -> Int
partA input = sum $ map scoreLine input

groupsOfThree :: [String] -> [(String, String, String)]
groupsOfThree [] = []
groupsOfThree (a : b : c : xs) = (a, b, c) : groupsOfThree xs
groupsOfThree _ = error "groupsOfThree: input list length not a multiple of 3"

inCommonThree :: (String, String, String) -> Char
inCommonThree (a, b, c) = head $ inCommon (inCommon (a, b), c)

partB :: Input -> Int
partB input = sum $ map (score . inCommonThree) (groupsOfThree input)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: "
      print $ partB input
