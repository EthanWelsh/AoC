module Day12 (solve) where

import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils.Maze (Maze, mazeFromDimensions, mazeFromList)
import Utils.Parsers (Parser, integer)

type Grid = Maze Char

type Present = Grid

newtype Tree = Tree  (Grid, [Int]) deriving (Show)

type Input = ([Present], [Tree])

presentParser :: Parser Grid
presentParser = try $ do
  _ <- integer
  _ <- string ":\n"
  chars <- some (choice [char '#', char '.']) `sepEndBy` newline
  return $ mazeFromList chars

treeParser :: Parser Tree
treeParser = do
  w <- decimal
  _ <- char 'x'
  h <- decimal
  _ <- string ": "
  nums <- decimal `sepBy` char ' '
  return $ Tree (mazeFromDimensions w h '.', nums)

parseInput :: Parser Input
parseInput = do
  presents <- presentParser `sepEndBy` newline
  _ <- many newline
  trees <- treeParser `sepEndBy` newline
  return (presents, trees)

part1 :: Input -> IO ()
part1 (gifts, trees) = do
  putStr "Part 1: "
  print $ length gifts
  print $ trees !! 1

part2 :: Input -> IO ()
part2 _ = do
  putStr "Part 2: "

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
