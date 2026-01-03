{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Year2021.Day04 (solve) where

import Control.Monad (void)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, take, transpose)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, errorBundlePretty, many, optional, parse, sepBy1, some)
import Text.Megaparsec.Char (char, eol, hspace)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

type Board = [[Int]]

type Calls = [Int]

data Bingo = Bingo
  { draws :: [Int],
    boards :: [Board]
  }

type Input = Bingo

localLexeme :: Parser a -> Parser a
localLexeme = L.lexeme hspace

localInteger :: Parser Int
localInteger = do
  sign_ <- maybe 1 (const (-1)) <$> optional (char '-')
  abs_ <- localLexeme L.decimal
  return (sign_ * abs_)

parseDraws :: Parser [Int]
parseDraws = localInteger `sepBy1` (localLexeme (char ','))

parseLine :: Parser [Int]
parseLine = do
  void $ many hspace -- Use hspace for horizontal space
  line <- localInteger `sepBy1` (some hspace) -- Numbers separated by hspace
  return line

parseBoard :: Parser [[Int]]
parseBoard = count 5 (parseLine <* eol)

inputParser :: Parser Input
inputParser = do
  ds <- parseDraws
  _ <- eol >> eol
  bs <- many (parseBoard <* optional eol)
  return $ Bingo ds bs

getRowsAndCols :: Board -> [[Int]]
getRowsAndCols b = b ++ (transpose b)

isWin :: Set Int -> Board -> Bool
isWin calls b = any (all (`Set.member` calls)) (getRowsAndCols b)

getIncrementalSets :: Calls -> [(Set Int, Set Int)]
getIncrementalSets c =
  let callsAsSets = map (Set.fromList . flip Data.List.take c) [0 .. length c]
   in zip callsAsSets (Set.empty : callsAsSets)

winningBoards :: (Set Int, Set Int) -> [Board] -> (Set Int, [Board])
winningBoards (currentCalls, prevCalls) bs =
  let isNewWin b = isWin currentCalls b && not (isWin prevCalls b)
   in (currentCalls, filter isNewWin bs)

getCallsToWinEachBoard :: [Board] -> Calls -> [(Set Int, Board)]
getCallsToWinEachBoard bs c =
  let callSets = getIncrementalSets c
      winsAtEachCall = map (`winningBoards` bs) callSets
      f (c', bs') = map (\b -> (c', b)) bs'
   in concatMap f winsAtEachCall

firstToWin :: [Board] -> Calls -> (Set Int, Board)
firstToWin bs c =
  let callsToWinEachBoard = getCallsToWinEachBoard bs c
   in minimumBy (compare `on` (Set.size . fst)) callsToWinEachBoard

notCalled :: Board -> Set Int -> [Int]
notCalled b c = filter (\cell -> not (cell `Set.member` c)) (concat b)

score :: (Set Int, Board) -> [Int] -> Int
score (c_set, b) all_calls = (sum (notCalled b c_set)) * (all_calls !! (Set.size c_set - 1))

partA :: Input -> String
partA (Bingo c bs) = show $ score (firstToWin bs c) c

lastToWin :: [Board] -> Calls -> (Set Int, Board)
lastToWin bs c =
  let callsToWinEachBoard = getCallsToWinEachBoard bs c
   in maximumBy (compare `on` (Set.size . fst)) callsToWinEachBoard

partB :: Input -> String
partB (Bingo c bs) = show $ score (lastToWin bs c) c

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
