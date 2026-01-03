{-# LANGUAGE OverloadedStrings #-}

module Year2022.Day10 (solve) where

{- ORMOLU_DISABLE -}
import Data.List (concatMap, scanl, zip)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

type Parser = Parsec Void T.Text

------------ PARSER ------------
noopParser :: Parser Op
noopParser = string "noop" $> Noop

addxParser :: Parser Op
addxParser = do
  _ <- string "addx "
  Addx <$> L.signed (pure ()) L.decimal

inputParser :: Parser Input
inputParser = (noopParser <|> addxParser) `sepBy` eol

------------ TYPES ------------
data Op = Noop | Addx Int deriving (Show, Eq)

type Input = [Op]

type OutputA = Int

type OutputB = String

------------ PART A ------------
opToCycles :: Op -> [Int]
opToCycles Noop = [0]
opToCycles (Addx n) = [0, n]

getHistory :: [Op] -> [Int]
getHistory ops =
  let cycles = concatMap opToCycles ops
   in scanl (+) 1 cycles

partA :: Input -> OutputA
partA input =
  let history = getHistory input
      points = [20, 60, 100, 140, 180, 220]
      strengths = map (\p -> (history !! (p - 1)) * p) points
   in sum strengths

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let history = getHistory input
      screen = map (\(p, s) -> if abs (p - s) <= 1 then '#' else '.') (zip (cycle [0 .. 39]) history)
   in unlines $ chunks 40 screen

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

solve :: FilePath -> IO ()
solve filePath = do
  contents <- TIO.readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: \n"
      putStrLn $ partB input
