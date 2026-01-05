module Year2023.Day08 (solve) where

import Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import Parsers (Parser, charInRange)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, string)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example1_1 = unsafePerformIO $ readFile "years/Year2023/input/sample/Day08_part1_1.txt"
-- >>> let Right parsedExample1_1 = parse parseInput "" example1_1
-- >>> let example1_2 = unsafePerformIO $ readFile "years/Year2023/input/sample/Day08_part1_2.txt"
-- >>> let Right parsedExample1_2 = parse parseInput "" example1_2
-- >>> let example2 = unsafePerformIO $ readFile "years/Year2023/input/sample/Day08_part2.txt"
-- >>> let Right parsedExample2 = parse parseInput "" example2
-- >>> partA parsedExample1_1
-- 2
-- >>> partA parsedExample1_2
-- 6
-- >>> partB parsedExample2
-- 6

data Direction = L | R deriving (Show, Eq)

type Room = String

type Maze = HM.HashMap Room (Room, Room)

data Input = Input {dirs :: [Direction], maze :: Maze} deriving (Show)

directionParser :: Parser Direction
directionParser = choice [R <$ char 'R', L <$ char 'L']

roomParser :: Parser Room
roomParser = count 3 (charInRange 'A' 'Z' <|> charInRange '0' '9')

lineParser :: Parser (Room, (Room, Room))
lineParser = do
  src <- roomParser
  void $ string " = ("
  l <- roomParser
  void $ string ", "
  r <- roomParser
  void $ string ")"
  return (src, (l, r))

parseInput :: Parser Input
parseInput = do
  dirs <- manyTill directionParser newline
  void $ newline
  ls <- lineParser `sepEndBy` newline
  let m = HM.fromList ls
  return (Input dirs m)

step :: Maze -> Direction -> Room -> Room
step m L r = fst (m HM.! r)
step m R r = snd (m HM.! r)

stepsToEnd :: Maze -> (Room -> Bool) -> [Direction] -> Room -> Int
stepsToEnd _ _ [] _ = error "Directions should be infinite"
stepsToEnd m isEnd (d : ds) r = if isEnd r then 0 else 1 + remainingSteps
  where
    remainingSteps = stepsToEnd m isEnd ds (step m d r)

partA :: Input -> Int
partA input =
  let ds = (concat . repeat) $ dirs input :: [Direction]
      m = maze input
   in stepsToEnd m (== "ZZZ") ds "AAA"

stepsToEndPt2 :: Maze -> [Room] -> [Direction] -> Int
stepsToEndPt2 m rs ds = foldl1 lcm $ map (stepsToEnd m isEnd ds) rs
  where
    isEnd room = 'Z' == last room

partB :: Input -> Int
partB input =
  let ds = (concat . repeat) $ dirs input :: [Direction]
      m = maze input
      starts = filter ((== 'A') . last) (HM.keys m)
   in stepsToEndPt2 m starts ds

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStrLn $ "Part 1: " ++ show (partA input)
      putStrLn $ "Part 2: " ++ show (partB input)