module Year2022.Day08 (solve) where

{- ORMOLU_DISABLE -}
import Matrix
import Data.Void (Void)
import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

-- $setup
-- >>> import qualified Data.Text.IO as TIO
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ TIO.readFile "years/Year2022/input/sample/Day08.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- 21
-- >>> partB parsedExample
-- 8

type Parser = Parsec Void T.Text

------------ PARSER ------------
inputParser :: Parser Input
inputParser = map T.unpack . T.lines <$> takeRest

------------ TYPES ------------
type Trees = Matrix Char

type Input = Trees

type OutputA = Int

type OutputB = Int

------------ PART A ------------

range :: Int -> Int -> [Int]
range start end =
  if start < end
    then [start .. end]
    else
      if start > end
        then [start, start - 1 .. end]
        else -- start == end
          [start]

getLinesOfSite :: Trees -> Point -> [[Point]]
getLinesOfSite ts (r, c) =
  let height = (getHeight ts) - 1
      width = (getWidth ts) - 1
      up = zip (range r 0) (repeat c)
      down = zip (range r height) (repeat c)
      left = zip (repeat r) (range c 0)
      right = zip (repeat r) (range c width)
   in [up, down, left, right]

isStrictlyDecreasing :: (Ord a) => [a] -> Bool
isStrictlyDecreasing [] = True
isStrictlyDecreasing (x : xs) = all (< x) xs

isVisible :: Trees -> Point -> Bool
isVisible ts p =
  let linesOfSitePoints = getLinesOfSite ts p :: [[Point]]
      linesOfSiteValues = map (map (get ts)) linesOfSitePoints :: [[Char]]
      isDecreasing = map isStrictlyDecreasing linesOfSiteValues :: [Bool]
   in or isDecreasing

countWhere :: (a -> Bool) -> [a] -> Int
countWhere p xs = length $ filter p xs

partA :: Input -> OutputA
partA trees =
  let allPoints = getAllPoints trees
   in countWhere (isVisible trees) allPoints

------------ PART B ------------

countDecreasingFrom :: (Ord a) => a -> [a] -> Int
countDecreasingFrom _ [] = 0
countDecreasingFrom t (x : xs) = if x >= t then 1 else 1 + countDecreasingFrom t xs

countDecreasing :: (Ord a) => [a] -> Int
countDecreasing [] = 0
countDecreasing (x : xs) = countDecreasingFrom x xs

getScore :: Trees -> Point -> Int
getScore ts p =
  let linesOfSitePoints = getLinesOfSite ts p :: [[Point]]
      linesOfSiteValues = map (map (get ts)) linesOfSitePoints :: [[Char]]
   in product $ map countDecreasing linesOfSiteValues

partB :: Input -> OutputB
partB ts =
  let allPoints = getAllPoints ts :: [Point]
      allScores = map (getScore ts) allPoints :: [Int]
   in maximum allScores

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
