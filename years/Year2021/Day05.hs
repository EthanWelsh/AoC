{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2021.Day05 (solve) where

{- ORMOLU_DISABLE -}
import Data.List (foldl, filter, concatMap, zip, length)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}


type Parser = Parsec Void T.Text

------------ PARSER ------------

pointParser :: Parser Point
pointParser = do
    a <- L.decimal
    void $ char ','
    b <- L.decimal
    return (a, b)

arrowParser :: Parser Arrow
arrowParser = do
    a <- pointParser
    void $ string " -> "
    b <- pointParser
    return (Arrow a b)

inputParser :: Parser Input
inputParser = arrowParser `sepBy` eol


------------ TYPES ------------
type Point = (Int, Int)
data Arrow = Arrow Point Point deriving (Eq, Show)

type Input = [Arrow]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

range :: Int -> Int -> [Int]
range start end
    | start < end  = [start..end]
    | start > end  = [start, start - 1 .. end]
    | start == end = [start] 

getPoints :: Arrow -> [Point]
getPoints (Arrow (x1, y1) (x2, y2)) = zip (range x1 x2) (range y1 y2)


getDupesPerPoint :: [Point] -> Map Point Int
getDupesPerPoint ps = foldl (\acc point -> Map.insertWith (+) point 1 acc) Map.empty ps

numberOfDupes :: [Point] -> Int
numberOfDupes ps = let
    dupesPerPoint = getDupesPerPoint ps    :: Map Point Int
    dupesAsList = Map.toList dupesPerPoint :: [(Point, Int)]
    in length $ filter ((>1) . snd) dupesAsList


notDiagonal :: Arrow -> Bool
notDiagonal (Arrow (x1, y1) (x2, y2)) = if x1 == x2 || y1 == y2 then True else False

partA :: Input -> OutputA
partA input = let
    noDiagonals = filter (notDiagonal) input :: [Arrow]
    allPoints = concatMap getPoints noDiagonals   :: [Point]
    in numberOfDupes allPoints

------------ PART B ------------
partB :: Input -> OutputB
partB input = let
    allPoints = concatMap getPoints input :: [Point]
    in numberOfDupes allPoints

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
