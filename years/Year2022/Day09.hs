module Year2022.Day09 (solve) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.List
import           Data.Maybe
import           Matrix
import           Parsers              (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Input = [CardinalDirection]

dirParser :: Char -> CardinalDirection -> Parser [CardinalDirection]
dirParser c d = do
    void $ char c
    void $ char ' '
    count <- L.decimal
    return $ replicate count d

dirsParser :: Parser [CardinalDirection]
dirsParser = (dirParser 'U' North) <|> (dirParser 'D' South) <|> (dirParser 'L' West) <|> (dirParser 'R' East)

inputParser :: Parser Input
inputParser = do
    dirs <- dirsParser `sepEndBy` eol
    return $ concat dirs

areTouching :: Point -> Point -> Bool
areTouching (a, b) (x, y) = if abs (a - x) <= 1 && abs (b - y) <= 1 then True else False

getTailLocs :: [Point] -> [Point]
getTailLocs = reverse . foldl update []
  where
    update [] v = [v]
    update pos@((x', y'):_) (x, y)
      | areTouching (x', y') (x, y) = pos
      | otherwise = (x' + signum (x-x'), y' + signum (y-y')):pos

partA :: Input -> String
partA input = let
    headLocs = getPath (0, 0) input
    in show $ length . nub . flip (!!) 1 . iterate getTailLocs $ headLocs

partB :: Input -> String
partB input = let
    headLocs = getPath (0, 0) input
    in show $ length . nub . flip (!!) 9 . iterate getTailLocs $ headLocs

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