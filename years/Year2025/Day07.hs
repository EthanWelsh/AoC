module Year2025.Day07 (solve) where

import Data.Function.Memoize (memoize)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)
import Maze
import Parsers (Parser)

type Grid = Maze Char

type Input = Grid

parseInput :: Parser Input
parseInput = do
  let lineP = some (char 'S' <|> char '.' <|> char '^')
  ls <- lineP `sepEndBy1` eol
  return $ mazeFromList ls

countSplits :: Grid -> Point -> Set Point
countSplits g = go
  where
    go = memoize $ \p ->
      case maybeGetPoint g p of
          Nothing -> Set.empty
          Just '.' -> go (south p)
          Just 'S' -> go (south p)
          Just '^' -> Set.unions [Set.singleton p, go (east p), go (west p)]
          _ -> error "Unexpected point"

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let start = head $ findPoints input (== 'S')
  let splits = countSplits input start
  print $ Set.size splits

countPaths :: Grid -> Point -> Int
countPaths g = go
  where
    go = memoize $ \p ->
      case maybeGetPoint g p of
          Nothing -> 0
          Just '.' -> go (south p)
          Just 'S' -> go (south p)
          Just '^' -> 1 + go (east p) + go (west p)
          _ -> error "Unexpected point" 

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let start = head $ findPoints input (== 'S')
  print $ 1 + countPaths input start

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
