module Year2023.Day02 (solve) where

import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2023/input/sample/Day02.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- 8
-- >>> partB parsedExample
-- 2286

type RGB = (Int, Int, Int)

data Game = G Int [RGB] deriving (Show)

number :: Parser Int
number = L.decimal

rgbParser :: Parser RGB
rgbParser = do
  n <- number
  _ <- string " "
  color <- string "red" <|> string "green" <|> string "blue"
  case color of
    "red" -> return (n, 0, 0)
    "green" -> return (0, n, 0)
    "blue" -> return (0, 0, n)
    _ -> error "unexpected color"

roundParser :: Parser RGB
roundParser = do
  uncollapsed <- rgbParser `sepBy` string ", "
  return $ addRounds uncollapsed

roundsParser :: Parser [RGB]
roundsParser = roundParser `sepBy` string "; "

gameParser :: Parser Game
gameParser = do
  _ <- string "Game "
  n <- number
  _ <- string ": "
  rounds <- roundsParser
  return (G n rounds)

inputParser :: Parser [Game]
inputParser = gameParser `sepEndBy` newline

addRounds :: [RGB] -> RGB
addRounds = foldl addRound (0, 0, 0)
  where
    addRound (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

maxRounds :: [RGB] -> RGB
maxRounds = foldl maxRound (0, 0, 0)
  where
    maxRound (a1, a2, a3) (b1, b2, b3) = (max a1 b1, max a2 b2, max a3 b3)

gameIsPossible :: Game -> Bool
gameIsPossible (G _ rounds) =
  let (r, g, b) = maxRounds rounds
   in r <= 12 && g <= 13 && b <= 14

sumOfGameIds :: [Game] -> Int
sumOfGameIds = foldl addGameId 0
  where
    addGameId a (G b _) = a + b

gamePower :: Game -> Int
gamePower (G _ rounds) =
  let (r, g, b) = maxRounds rounds
   in r * g * b

partA :: [Game] -> Int
partA games =
  let possibleGames = filter gameIsPossible games
   in sumOfGameIds possibleGames

partB :: [Game] -> Int
partB games =
  let powerValues = map gamePower games
   in sum powerValues

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right records -> do
      print $ partA records
      print $ partB records