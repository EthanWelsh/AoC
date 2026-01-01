module Year2022.Day06 (solve) where

import           Data.List
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Parsers (Parser)

type Input = String

inputParser :: Parser Input
inputParser = some anySingle

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf size xs = (Data.List.take size xs) : groupsOf size (drop 1 xs)

noDupes :: [Char] -> Bool
noDupes xs = (length xs) == length (nub xs)

findIndexOfFirstUniqueGroup :: Int -> String -> Int
findIndexOfFirstUniqueGroup groupSize input = let
    groups = groupsOf groupSize input                        :: [String]
    uniqueGroup = fromJust $ find noDupes groups             :: String
    groupIndex = fromJust $ findIndex (==uniqueGroup) groups :: Int
    in groupIndex + length uniqueGroup

partA :: Input -> Int
partA input = findIndexOfFirstUniqueGroup 4 input

partB :: Input -> Int
partB input = findIndexOfFirstUniqueGroup 14 input

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