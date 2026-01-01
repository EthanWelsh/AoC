{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Year2022.Day14 (solve) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.Function              (on)
import qualified "unordered-containers" Data.HashSet as HashSet
import           Data.List
import           Data.Maybe                 (fromJust)
import           Data.Functor               (($>))
import           Parsers                    (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Util

type Point = (Int, Int)
type Arrow = [Point]
type Blocks = HashSet.HashSet Point
type Input = [Arrow]

pointParser :: Parser Point
pointParser = do
    x <- L.decimal
    void $ char ','
    y <- L.decimal
    return (x, y)

arrowParser :: Parser Arrow
arrowParser = pointParser `sepBy` (string " -> ")

inputParser :: Parser Input
inputParser = arrowParser `sepEndBy` eol

down :: Point -> Point
down (x, y) =      (x, y + 1)
downLeft :: Point -> Point
downLeft (x, y) =  (x - 1, y + 1)
downRight :: Point -> Point
downRight (x, y) = (x + 1, y + 1)

isHigherThan :: Point -> Point -> Bool
isHigherThan (x, y) (x', y') = y' < y

isObstructed :: Point -> Blocks -> Bool
isObstructed point blocks = HashSet.member point blocks

notObstructed :: Point -> Blocks -> Bool
notObstructed point blocks = not $ isObstructed point blocks

inAbyss :: Point -> Blocks -> Bool
inAbyss point blocks = all (isHigherThan point) blocks

simulate :: Point -> Blocks -> (Point -> Blocks -> Bool) -> (Blocks, Bool)
simulate point blocks stopCondition
    | stopCondition point blocks    = (blocks, True)
    | notObstructed d blocks  = simulate d  blocks stopCondition
    | notObstructed dl blocks = simulate dl blocks stopCondition
    | notObstructed dr blocks = simulate dr blocks stopCondition
    | otherwise               = (HashSet.insert point blocks, False)
    where
        d = down point
        dl = downLeft point
        dr = downRight point

dropSand :: Blocks -> (Point -> Blocks -> Bool) -> (Blocks, Bool)
dropSand blocks stopCondition = simulate (500,0) blocks stopCondition

dropSandForever :: Blocks -> (Point -> Blocks -> Bool) -> [(Blocks, Bool)]
dropSandForever blocks stopCondition = iterate helper (blocks, False)
  where helper (blocks, _) = dropSand blocks stopCondition

arrowToSegment :: Arrow -> [(Point, Point)]
arrowToSegment arrow = laggedPairs arrow

segmentBlocks :: (Point, Point) -> Blocks
segmentBlocks ((a, b), (c, d)) = HashSet.fromList $ zip (range a c) (range b d)

unionAll :: [Blocks] -> Blocks
unionAll blocks = HashSet.unions blocks

blocksFromArrow :: Arrow -> Blocks
blocksFromArrow arrow = unionAll $ map segmentBlocks (arrowToSegment arrow)

blocksFromArrows :: [Arrow] -> Blocks
blocksFromArrows arrows = unionAll $ map blocksFromArrow arrows

partA :: Input -> String
partA input = show $ (fromJust $ findIndex (snd) (dropSandForever (blocksFromArrows input) inAbyss)) - 1

lowestPoint :: Blocks -> Point
lowestPoint blocks = maximumBy (compare `on` snd) (HashSet.toList blocks)

addFloor :: Blocks -> Blocks
addFloor blocks = let
    (x, y) = lowestPoint blocks
    floorY = y + 2
    floorPoints = zip (range (-1000) 1000) (repeat floorY) -- this is practically infinite
    in HashSet.union blocks (HashSet.fromList floorPoints)

startBlocked :: Point -> Blocks -> Bool
startBlocked _ blocks = HashSet.member (500, 0) blocks

partB :: Input -> String
partB input = let
    blocks = blocksFromArrows input
    blocksWithFloor = addFloor blocks
    in show $ (fromJust $ findIndex (snd) (dropSandForever blocksWithFloor startBlocked)) - 1

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      putStrLn $ partA input
      putStr "Part 2: "
      putStrLn $ partB input