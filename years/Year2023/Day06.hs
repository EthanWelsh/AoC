module Year2023.Day06 (solve) where

import Control.Monad (void)
import Parsers (Parser, integer, skipSpaces)
import Text.Megaparsec
import Text.Megaparsec.Char (string)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2023/input/sample/Day06.txt"
-- >>> let Right parsedExample = parse parseInput "" example
-- >>> partA parsedExample
-- 288
-- >>> partB parsedExample
-- 71503

type Input = [Race]

data Race = Race
  { time :: Int,
    distance :: Int
  }
  deriving (Show, Eq)

data Action = Action
  { holdTime :: Int,
    remainingTime :: Int
  }
  deriving (Show)

parseInput :: Parser Input
parseInput = do
  void $ string "Time:"
  skipSpaces
  times <- many integer
  void $ string "Distance:"
  skipSpaces
  distances <- many integer
  return $ zipWith Race times distances

calculateScore :: Action -> Int
calculateScore a = remainingTime a * holdTime a

possibleActions :: Race -> [Action]
possibleActions race = map (\ht -> Action ht (rt - ht)) holdTimes
  where
    rt = time race
    holdTimes = [1 .. rt]

waysToBeat :: Race -> Int
waysToBeat race =
  let actions = possibleActions race
      scores = map calculateScore actions
      highScore = distance race
   in length $ filter (> highScore) scores

partA :: Input -> Int
partA input = product $ map waysToBeat input

combineDigits :: [Int] -> Int
combineDigits xs = read $ concatMap show xs

oneRace :: [Race] -> Race
oneRace rs =
  let ts = map time rs
      ds = map distance rs
   in Race (combineDigits ts) (combineDigits ds)

partB :: Input -> Int
partB input =
  let race = oneRace input
   in waysToBeat race

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: "
      print $ partB input