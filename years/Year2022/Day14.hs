{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Year2022.Day14 (solve) where

{- ORMOLU_DISABLE -}

import Control.Monad (void)
import Data.Function
import qualified "unordered-containers" Data.HashSet as HashSet
import Data.List (findIndex, maximumBy)
import Data.Maybe
import Data.Void
import qualified Util as U
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

-- $setup
-- >>> import qualified Data.Text.IO as TIO
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ TIO.readFile "years/Year2022/input/sample/Day14.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- 24
-- >>> partB parsedExample
-- 93

type Parser = Parsec Void T.Text

------------ PARSER ------------
pointParser :: Parser Point
pointParser = do
  x <- L.decimal
  void $ char ','
  y <- L.decimal
  return (x, y)

arrowParser :: Parser Arrow
arrowParser = pointParser `sepBy` string " -> "

inputParser :: Parser Input
inputParser = arrowParser `sepEndBy` eol

------------ TYPES ------------
type Point = (Int, Int)

type Arrow = [Point]

type Blocks = HashSet.HashSet Point

type Input = [Arrow]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
down :: Point -> Point
down (x, y) = (x, y + 1)

downLeft :: Point -> Point
downLeft (x, y) = (x - 1, y + 1)

downRight :: Point -> Point
downRight (x, y) = (x + 1, y + 1)

isHigherThan :: Point -> Point -> Bool
isHigherThan (_, y) (_x', y') = y' < y

isObstructed :: Point -> Blocks -> Bool
isObstructed point blocks = HashSet.member point blocks

notObstructed :: Point -> Blocks -> Bool
notObstructed point blocks = not $ isObstructed point blocks

inAbyss :: Point -> Blocks -> Bool
inAbyss point blocks = all (isHigherThan point) blocks

simulate :: Point -> Blocks -> (Point -> Blocks -> Bool) -> (Blocks, Bool)
simulate point blocks stopCondition
  | stopCondition point blocks = (blocks, True)
  | notObstructed d blocks = simulate d blocks stopCondition
  | notObstructed dl blocks = simulate dl blocks stopCondition
  | notObstructed dr blocks = simulate dr blocks stopCondition
  | otherwise = (HashSet.insert point blocks, False)
  where
    d = down point
    dl = downLeft point
    dr = downRight point

dropSand :: Blocks -> (Point -> Blocks -> Bool) -> (Blocks, Bool)
dropSand blocks stopCondition = simulate (500, 0) blocks stopCondition

dropSandForever :: Blocks -> (Point -> Blocks -> Bool) -> [(Blocks, Bool)]
dropSandForever blks stopCondition = iterate helper (blks, False)
  where
    helper (currentBlocks, _) = dropSand currentBlocks stopCondition

arrowToSegment :: Arrow -> [(Point, Point)]
arrowToSegment arrow = U.laggedPairs arrow

segmentBlocks :: (Point, Point) -> Blocks
segmentBlocks ((x1, y1), (x2, y2)) =
  HashSet.fromList $
    if x1 == x2
      then [(x1, y) | y <- U.range y1 y2]
      else [(x, y1) | x <- U.range x1 x2]

unionAll :: [Blocks] -> Blocks
unionAll blocks = HashSet.unions blocks

blocksFromArrow :: Arrow -> Blocks
blocksFromArrow arrow = unionAll $ map segmentBlocks (arrowToSegment arrow)

blocksFromArrows :: [Arrow] -> Blocks
blocksFromArrows arrows = unionAll $ map blocksFromArrow arrows

partA :: Input -> OutputA
partA input = (fromJust $ findIndex (snd) (dropSandForever (blocksFromArrows input) inAbyss)) - 1

------------ PART B ------------
lowestPoint :: Blocks -> Point
lowestPoint blocks = maximumBy (compare `on` snd) blocks

addFloor :: Blocks -> Blocks
addFloor blocks =
  let (_, y) = lowestPoint blocks
      floorY = y + 2
      floorPoints = zip (U.range (-1000) 1000) (repeat floorY) -- this is practically infinite
   in HashSet.union blocks (HashSet.fromList floorPoints)

startBlocked :: Point -> Blocks -> Bool
startBlocked _ blocks = HashSet.member (500, 0) blocks

partB :: Input -> OutputB
partB input =
  let blocks = blocksFromArrows input
      blocksWithFloor = addFloor blocks
   in (fromJust $ findIndex (snd) (dropSandForever blocksWithFloor startBlocked)) - 1

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
