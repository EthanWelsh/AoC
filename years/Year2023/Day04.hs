module Year2023.Day04 (solve) where

import Control.Monad (void)
import Data.List (intersect)
import Parsers (Parser, colon, integer, lexeme, pipe)
import Text.Megaparsec
import Text.Megaparsec.Char (string)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2023/input/sample/Day04.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- 13
-- >>> partB parsedExample
-- 30

data Card = C Int [Int] [Int] deriving (Show)

inputParser :: Parser [Card]
inputParser = many cardParser

parseCardId :: Parser Int
parseCardId =
  lexeme (string "Card")
    *> integer
    <* colon

cardParser :: Parser Card
cardParser = do
  n <- parseCardId
  wins <- many integer
  void pipe
  haves <- many integer
  return (C n wins haves)

winners :: Card -> [Int]
winners (C _ wins haves) = wins `intersect` haves

winCount :: Card -> Int
winCount card = length $ winners card

cardScore :: Card -> Int
cardScore c =
  let wins = winCount c
   in if wins == 0 then 0 else 2 ^ (wins - 1)

getCard :: [Card] -> Int -> Card
getCard cards n = cards !! n

cardNumber :: Card -> Int
cardNumber (C n _ _) = n

countCopies :: [Card] -> Card -> Int
countCopies cards card =
  let cardNum = cardNumber card
      numberOfWinners = length $ winners card
      firstCopyNum = cardNum + 1
      lastCopyNum = firstCopyNum + numberOfWinners - 1
      winningNums = if numberOfWinners > 0 then [firstCopyNum .. lastCopyNum] else []
      minusOne n = n - 1
      winningCards = map (getCard cards . minusOne) winningNums
      copyCount = sum $ map (countCopies cards) winningCards
   in 1 + copyCount

partA :: [Card] -> Int
partA = sum . map cardScore

partB :: [Card] -> Int
partB cards = sum $ map (countCopies cards) cards

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right cards -> do
      putStrLn $ "Part 1: " ++ show (partA cards)
      putStrLn $ "Part 2: " ++ show (partB cards)