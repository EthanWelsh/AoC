{-# LANGUAGE OverloadedStrings #-}
module Year2022.Day09 (solve) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Control.Monad (void)
import Control.Applicative ((<|>))
import Data.Functor (($>))

import Matrix

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

type Parser = Parsec Void T.Text

------------ PARSER ------------
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
  dirs <- dirsParser `sepBy` eol
  return $ concat dirs

------------ TYPES ------------
type Input = [CardinalDirection]

type OutputA = String

type OutputB = String

------------ PART A ------------
areTouching :: Point -> Point -> Bool
areTouching (a, b) (x, y) = if abs (a - x) <= 1 && abs (b - y) <= 1 then True else False

getTailLocs :: [Point] -> [Point]
getTailLocs = reverse . foldl update []
  where
    update [] v = [v]
    update pos@((x', y'):_) (x, y)
      | areTouching (x', y') (x, y) = pos
      | otherwise = (x' + signum (x-x'), y' + signum (y-y')):pos

partA :: Input -> OutputA
partA input =
  let
    headLocs = getPath (0, 0) input
  in show $ length . nub . flip (!!) 1 . iterate getTailLocs $ headLocs

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let
    headLocs = getPath (0, 0) input
  in show $ length . nub . flip (!!) 9 . iterate getTailLocs $ headLocs

solve :: FilePath -> IO ()
solve filePath = do
  contents <- TIO.readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      putStrLn $ partA input
      putStr "Part 2: "
      putStrLn $ partB input