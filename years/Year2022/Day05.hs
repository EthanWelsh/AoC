{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day05 (solve) where

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
import Data.Text (Text, pack)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void T.Text
{- ORMOLU_ENABLE -}

solve :: FilePath -> IO ()
solve filePath = do
    fileContents <- readFile filePath
    case parse inputParser "" (pack fileContents) of
        Left e -> putStrLn $ errorBundlePretty e
        Right input -> do
            putStrLn "Part A:"
            print (partA input)
            putStrLn "Part B:"
            print (partB input)

------------ PARSER ------------

pointParser :: Parser Point
pointParser = do
  x <- L.decimal
  void $ char ','
  b <- L.decimal
  return (x, b)

arrowParser :: Parser Arrow
arrowParser = do
    a <- pointParser
    void $ string (T.pack " -> ")
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
