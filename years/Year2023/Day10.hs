module Year2023.Day10 (solve) where

import Algorithm.Search
import qualified Data.List as List
import Data.Maybe (fromJust, isJust)
import Maze
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example1_1 = unsafePerformIO $ readFile "years/Year2023/input/sample/Day10_part1_1.txt"
-- >>> let Right parsedExample1_1 = parse parseInput "" example1_1
-- >>> let example1_2 = unsafePerformIO $ readFile "years/Year2023/input/sample/Day10_part1_2.txt"
-- >>> let Right parsedExample1_2 = parse parseInput "" example1_2
-- >>> partA parsedExample1_1
-- 4
-- >>> partA parsedExample1_2
-- 8

data Tile = Start | Vertical | Horizontal | Ground | NE | NW | SW | SE deriving (Eq)

type Board = Maze Tile

type Input = Board

instance Show Tile where
  show Start = "S"
  show Vertical = "|"
  show Horizontal = "-"
  show Ground = "."
  show NE = "L"
  show NW = "J"
  show SW = "7"
  show SE = "F"

parseTile :: Parser Tile
parseTile =
  choice
    [ Start <$ char 'S',
      Vertical <$ char '|',
      Horizontal <$ char '-',
      Ground <$ char '.',
      NE <$ char 'L',
      NW <$ char 'J',
      SW <$ char '7',
      SE <$ char 'F'
    ]

parseLine :: Parser [Tile]
parseLine = many parseTile

parseInput :: Parser Input
parseInput = do
  ls <- parseLine `sepEndBy` newline
  return $ mazeFromList ls

goesToDirs :: Tile -> [Direction]
goesToDirs Vertical = [North, South]
goesToDirs Horizontal = [East, West]
goesToDirs NE = [North, East]
goesToDirs NW = [North, West]
goesToDirs SW = [South, West]
goesToDirs SE = [South, East]
goesToDirs t = error $ "dirsAllowed: Unsupported tile=" ++ show t

acceptsFromDirs :: Tile -> [Direction]
acceptsFromDirs Vertical = [North, South]
acceptsFromDirs Horizontal = [East, West]
acceptsFromDirs NE = [South, West]
acceptsFromDirs NW = [South, East]
acceptsFromDirs SW = [North, East]
acceptsFromDirs SE = [North, West]
acceptsFromDirs Ground = []
acceptsFromDirs t = error $ "acceptsFrom: Unsupported tile=" ++ show t

isAllowed :: Board -> Point -> Direction -> Bool
isAllowed b p d = inBounds b newPoint && d `elem` goesToDirs oldTile && d `elem` (acceptsFromDirs newTile)
  where
    oldTile = getPoint b p
    newPoint = movePoint p d
    newTile = getPoint b newPoint

identifyStartTile :: Board -> Point -> Tile
identifyStartTile b p
  | onlyAllows [North, South] = Vertical
  | onlyAllows [East, West] = Horizontal
  | onlyAllows [North, East] = NE
  | onlyAllows [North, West] = NW
  | onlyAllows [South, West] = SW
  | onlyAllows [South, East] = SE
  | otherwise = error "Couldn't figure out starting tile"
  where
    onlyAllows dirs = doesAllow dirs && doesNotAllowOther dirs
    doesAllow = all (\d -> inBounds b (movePoint p d) && d `elem` acceptsFromDirs (getPoint b (movePoint p d)))
    otherDirs dirs = [North, South, East, West] List.\\ dirs
    doesNotAllowOther dirs = not $ doesAllow (otherDirs dirs)

step :: Board -> Point -> [Point]
step b p =
  let allowedDirs = filter (isAllowed b p) [North, East, South, West]
   in map (movePoint p) allowedDirs

longPaths :: Board -> [Point] -> Point -> [[Point]]
longPaths board visited point =
  let steps = step board point
      filteredSteps = filter (not . (`elem` visited)) steps
      noMoreSteps = null filteredSteps
      newVisited = point : visited
      paths = map (longPaths board newVisited) filteredSteps
   in if noMoreSteps then [visited] else concat paths

furthestPointInLoop :: Board -> Point -> Int
furthestPointInLoop b p =
  let paths = longPaths b [] p
      path1 = (tail . reverse) $ paths !! 0
      path2 = (tail . reverse) $ paths !! 1
      matchIndex = fromJust $ List.elemIndex True $ zipWith (==) path1 path2
   in 1 + matchIndex

partA :: Input -> Int
partA board =
  let startPoint = head $ findPoints board (== Start)
      startTile = identifyStartTile board startPoint
      m = setPoint board startPoint startTile
   in furthestPointInLoop m startPoint

markObviousNotGroundPoints :: Maze Char -> Maze Char
markObviousNotGroundPoints m =
  let ps = notGroundPoints m
   in setPoints m ps ' '

bigger :: Tile -> [[Char]]
bigger Vertical =
  [
    " # ",
    " # ",
    " # "
  ]
bigger Horizontal =
  [
    "   ",
    "###",
    "   "
  ]
bigger Ground =
  [
    "GGG",
    "GGG",
    "GGG"
  ]
bigger NE =
  [
    " # ",
    " ##",
    "   "
  ]
bigger NW =
  [
    " # ",
    "## ",
    "   "
  ]
bigger SW =
  [
    "   ",
    "## ",
    " # "
  ]
bigger SE =
  [
    "   ",
    " ##",
    " # "
  ]
bigger _ = error "Unknown tile passed to bigger."

biggerMaze :: Maze Tile -> Maze Char
biggerMaze m =
  let appendAll = zipWith (++)
      biggerLine [] = []
      biggerLine (x : xs) = foldl appendAll (bigger x) (map bigger xs)
      biggerM = mazeFromList $ concatMap biggerLine (mazeToList m)
   in markObviousNotGroundPoints biggerM

distanceFromEdge :: Maze Char -> Point -> Int
distanceFromEdge m (r, c) =
  let distanceFromTop = r + 1
      distanceFromLeft = c + 1
      distanceFromBottom = abs $ height m - r
      distanceFromRight = abs $ width m - c
      verticalDistance = min distanceFromTop distanceFromBottom
      horizontalDistance = min distanceFromLeft distanceFromRight
   in verticalDistance + horizontalDistance

neighbors :: Maze Char -> Point -> [Point]
neighbors m p =
  let ps = neighbors4 m p
   in filter (not . testPoint m (== '#')) ps

isEnd :: Maze Char -> Point -> Bool
isEnd m p = not (all (inBounds m . movePoint p) [North, East, South, West])

hasPathToOutside :: Maze Char -> Point -> Bool
hasPathToOutside m p = isJust $ aStar (neighbors m) (\_ _ -> 1) (distanceFromEdge m) (isEnd m) p

notGroundPoints :: Maze Char -> [Point]
notGroundPoints m =
  let pointsInRow mm r = [(r, c) | c <- [0 .. width mm - 1]]
      pointsInCol mm c = [(r, c) | r <- [0 .. height mm - 1]]
      takeWhileOuter f xs = takeWhile f xs ++ takeWhile f (reverse xs)
      rows = [0 .. height m - 1] :: [Int]
      cols = [0 .. width m - 1]
      rowPoints = map (pointsInRow m) rows :: [[Point]]
      horizontalScan = concatMap (takeWhileOuter (testPoint m (== 'G'))) rowPoints :: [Point]
      colPoints = map (pointsInCol m) cols
      verticalScan = concatMap (takeWhileOuter (testPoint m (== 'G'))) colPoints :: [Point]
   in horizontalScan ++ verticalScan

countGroundPointsInLoop :: Maze Tile -> Int
countGroundPointsInLoop m =
  let biggerM = biggerMaze m
      groundPoints = filter (testPoint biggerM (== 'G')) (allPoints biggerM)
      pointsWithoutPath = filter (not . hasPathToOutside biggerM) groundPoints
   in div (length pointsWithoutPath) 9

partB :: Input -> Int
partB board =
  let startPoint = head $ findPoints board (== Start)
      startTile = identifyStartTile board startPoint
      m = setPoint board startPoint startTile
      perimeterPoints = (List.sort . List.nub . concat) $ longPaths m [] startPoint
      otherPoints = allPoints board List.\\ perimeterPoints
      mm = setPoints m otherPoints Ground
   in countGroundPointsInLoop mm

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStrLn $ "Part 1: " ++ show (partA input)
      putStrLn $ "Part 2: " ++ show (partB input)
