module Year2022.Day02 (solve) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Parsers (Parser)

data Shape = Rock | Paper | Scissor
  deriving (Eq, Show)

data GameState = Win | Lose | Draw

data FirstColumn  = A | B | C
  deriving (Eq, Show)

data SecondColumn = X | Y | Z
  deriving (Eq, Show)

type Input = [(FirstColumn, SecondColumn)]

charToFirst :: Char -> FirstColumn
charToFirst 'A' = A
charToFirst 'B' = B
charToFirst 'C' = C

charToSecond :: Char -> SecondColumn
charToSecond 'X' = X
charToSecond 'Y' = Y
charToSecond 'Z' = Z

row :: Parser (FirstColumn, SecondColumn)
row = do
    first <- char 'A' <|> char 'B' <|> char 'C'
    void $ char ' '
    second <- char 'X' <|> char 'Y' <|> char 'Z'
    return (charToFirst first, charToSecond second)

inputParser :: Parser Input
inputParser = row `sepEndBy` eol

firstToShape :: FirstColumn -> Shape
firstToShape A = Rock
firstToShape B = Paper
firstToShape C = Scissor

secondToShape :: SecondColumn -> Shape
secondToShape X = Rock
secondToShape Y = Paper
secondToShape Z = Scissor

playGame :: (FirstColumn -> Shape) -> (SecondColumn -> Shape) -> (FirstColumn, SecondColumn) -> (Shape, Shape)
playGame fcToShape scToShape (first, second) = (fcToShape first, scToShape second)

playGames :: [(FirstColumn, SecondColumn)] -> (FirstColumn -> Shape) -> (SecondColumn -> Shape) -> [(Shape, Shape)]
playGames games fcToShape scToShape = map (playGame fcToShape scToShape) games

gameState :: (Shape, Shape) -> GameState
gameState (Rock,    Rock)    = Draw
gameState (Paper,   Paper)   = Draw
gameState (Scissor, Scissor) = Draw
gameState (Rock,    Paper)   = Win
gameState (Paper,   Scissor) = Win
gameState (Scissor, Rock)    = Win
gameState (Rock,    Scissor) = Lose
gameState (Paper,   Rock)    = Lose
gameState (Scissor, Paper)   = Lose

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissor = 3

gameStateScore :: GameState -> Int
gameStateScore Win = 6
gameStateScore Draw = 3
gameStateScore Lose = 0

gameScore :: (Shape, Shape) -> Int
gameScore (theirs, mine) = shapeScore mine + gameStateScore (gameState (theirs, mine))

scoreGames :: [(Shape, Shape)] -> Int
scoreGames games = sum $ map gameScore games

partA :: Input -> Int
partA input = scoreGames $ playGames input firstToShape secondToShape

pickRightShape :: Shape -> GameState -> Shape
pickRightShape Rock    Lose = Scissor
pickRightShape Rock    Draw = Rock
pickRightShape Rock    Win  = Paper
pickRightShape Paper   Lose = Rock
pickRightShape Paper   Draw = Paper
pickRightShape Paper   Win  = Scissor
pickRightShape Scissor Lose = Paper
pickRightShape Scissor Draw = Scissor
pickRightShape Scissor Win  = Rock

secondToGameState :: SecondColumn -> GameState
secondToGameState X = Lose
secondToGameState Y = Draw
secondToGameState Z = Win

toShapeAndGameState :: (FirstColumn, SecondColumn) -> (Shape, GameState)
toShapeAndGameState (fc, sc) = (firstToShape fc, secondToGameState sc)

toShapes :: (Shape, GameState) -> (Shape, Shape)
toShapes (shape, gameState) = (shape, pickRightShape shape gameState)

partB :: Input -> Int
partB input = let a = map toShapeAndGameState input
                  b = map toShapes a
                  in scoreGames b

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