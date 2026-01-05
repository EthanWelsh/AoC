module Year2022.Day06 (solve) where

{- ORMOLU_DISABLE -}
import Data.List (take, drop, length, nub, find, findIndex)
import Data.Maybe
import Data.Void (Void)
import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

-- $setup
-- >>> import qualified Data.Text as T
-- >>> partA (T.pack "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
-- 7
-- >>> partA (T.pack "bvwbjplbgvbhsrlpgdmjqwftvncz")
-- 5
-- >>> partA (T.pack "nppdvjthqldpwncqszvftbrmjlhg")
-- 6
-- >>> partA (T.pack "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
-- 10
-- >>> partA (T.pack "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
-- 11
-- >>> partB (T.pack "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
-- 19
-- >>> partB (T.pack "bvwbjplbgvbhsrlpgdmjqwftvncz")
-- 23
-- >>> partB (T.pack "nppdvjthqldpwncqszvftbrmjlhg")
-- 23
-- >>> partB (T.pack "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
-- 29
-- >>> partB (T.pack "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
-- 26

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
findIndexOfFirstUniqueGroup groupSize input =
  let groups = groupsOf groupSize input :: [String]
      uniqueGroup = fromJust $ find noDupes groups :: String
      groupIndex = fromJust $ findIndex (== uniqueGroup) groups :: Int
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
