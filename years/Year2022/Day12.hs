module Year2022.Day12 (solve) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Matrix
import Parsers (coordinateParser)
import Data.Char (ord)
import Search (bfs)
import Data.Void
import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

type Parser = Parsec Void String

------------ PARSER ------------
mapper :: Char -> Maybe Int
mapper 'S' = Just 0
mapper 'E' = Just 27
mapper c = Just $ (ord c) - 96

inputParser :: Parser Input
inputParser = coordinateParser mapper (0, 0)

------------ TYPES ------------
type Input = Map Point Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------

neighbors :: Point -> [Point]
neighbors (r, c) = [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]

getValidNeighbors :: (Point -> Point -> Bool) -> Point -> [Point]
getValidNeighbors canMove from = filter (canMove from) (neighbors from)

partA :: Input -> OutputA
partA input = fromJust $ bfs (getValidNeighbors (canMoveUp input)) (isEnd input) (findStart input)

canMoveUp :: Map Point Int -> Point -> Point -> Bool
canMoveUp m from to = let
    fromVal = fromJust $ Map.lookup from m
    toVal = fromJust $ Map.lookup to m
    in toVal <= (fromVal + 1)

isEnd :: Map Point Int -> Point -> Bool
isEnd m p = (fromJust $ Map.lookup p m) == 27

findStart :: Map Point Int -> Point
findStart m = let
    Just (p, _) = find (\(_, v) -> v == 0) (Map.toList m)
    in p

------------ PART B ------------
partB :: Input -> OutputB
partB input = fromJust $ bfs (getValidNeighbors (canMoveDown input)) (isStart input) (findEnd input)

canMoveDown :: Map Point Int -> Point -> Point -> Bool
canMoveDown m from to = let
    fromVal = fromJust $ Map.lookup from m
    toVal = fromJust $ Map.lookup to m
    in fromVal <= (toVal + 1)

isStart :: Map Point Int -> Point -> Bool
isStart m p = (fromJust $ Map.lookup p m) == 1

findEnd :: Map Point Int -> Point
findEnd m = let
    Just (p, _) = find (\(_, v) -> v == 27) (Map.toList m)
    in p

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: "
      print $ partB input