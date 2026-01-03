{-# LANGUAGE OverloadedStrings #-}

module Year2022.Day11 (solve) where

{- ORMOLU_DISABLE -}
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
monkeyIdParser :: Parser Int
monkeyIdParser = string "Monkey " *> L.decimal <* char ':' <* eol

startingItemsParser :: Parser [Int]
startingItemsParser = string "  Starting items: " *> (L.decimal `sepBy` string ", ") <* eol

operationParser :: Parser (Int -> Int)
operationParser = do
  _ <- string "  Operation: new = old "
  op <- (char '*' $> (*)) <|> (char '+' $> (+))
  _ <- string " "
  val <- (string "old" $> (-1)) <|> L.decimal
  eol
  return (\x -> let v = if val == -1 then x else val in op x v)

testParser :: Parser (Int, Int, Int)
testParser = do
  string "  Test: divisible by "
  d <- L.decimal
  eol
  string "    If true: throw to monkey "
  t <- L.decimal
  eol
  string "    If false: throw to monkey "
  f <- L.decimal
  eol
  return (d, t, f)

monkeyParser :: Parser Monkey
monkeyParser = do
  id <- monkeyIdParser
  items <- startingItemsParser
  op <- operationParser
  (d, t, f) <- testParser
  return
    Monkey
      { monkeyId = id,
        items = items,
        operation = op,
        test = (\x -> if x `mod` d == 0 then t else f)
      }

inputParser :: Parser Input
inputParser = monkeyParser `sepBy` eol

------------ TYPES ------------
data Monkey = Monkey
  { monkeyId :: Int,
    items :: [Int],
    operation :: (Int -> Int),
    test :: (Int -> Int)
  }

type Input = [Monkey]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"

solve :: FilePath -> IO ()
solve filePath = do
  contents <- TIO.readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right _ -> putStrLn "Parser successful, but solution not implemented."
