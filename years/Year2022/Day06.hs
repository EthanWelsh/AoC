module Year2022.Day06 (solve) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

type Parser = Parsec Void T.Text

------------ PARSER ------------
inputParser :: Parser Input
inputParser = takeRest

------------ TYPES ------------
type Input = T.Text

type OutputA = Int

type OutputB = Int

------------ PART A ------------

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

partA :: Input -> OutputA
partA input = findIndexOfFirstUniqueGroup 4 (T.unpack input)

------------ PART B ------------
partB :: Input -> OutputB
partB input = findIndexOfFirstUniqueGroup 14 (T.unpack input)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- TIO.readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: "
      print $ partB input