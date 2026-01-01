module Year2022.Day21 (solve) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Parsers                    (Parser, integer)
import           Text.Megaparsec            (parse, errorBundlePretty, anySingle, some, sepEndBy, try)
import           Text.Megaparsec.Char       (letterChar, eol, char, string)
import qualified Text.Megaparsec.Char.Lexer as L

data Operation = Number Int | Plus String String | Minus String String | Multiply String String | Divide String String deriving (Eq, Ord, Show)
type Input = Map.Map String Operation

nameParser :: Parser String
nameParser = some letterChar

numberParser :: Parser (Map.Map String Operation)
numberParser = do
    name <- nameParser
    void $ string ": "
    num <- integer
    return $ Map.fromList [(name, Number num)]

binaryOperationPartsParser :: Parser (String, Char, String)
binaryOperationPartsParser = do
    name1 <- nameParser
    void $ string " "
    op <- anySingle
    void $ string " "
    name2 <- nameParser
    return (name1, op, name2)

binaryOperationParser :: Parser (Map.Map String Operation)
binaryOperationParser = do
    name <- nameParser
    void $ string ": "
    (a, op, b) <- binaryOperationPartsParser
    let operation = case op of
                        '+' -> Plus a b
                        '-' -> Minus a b
                        '*' -> Multiply a b
                        '/' -> Divide a b
                        _   -> error "Unknown operation"
    return $ Map.fromList [(name, operation)]

inputParser :: Parser Input
inputParser = do
    ops <- (try numberParser <|> try binaryOperationParser) `sepEndBy` eol
    return $ Map.unions ops

solve' :: Map.Map String Operation -> String -> Int
solve' m name = let
    op = m Map.! name
    result = case op of
                Number n -> n
                Plus a b ->     (solve' m a) +     (solve' m b)
                Minus a b ->    (solve' m a) -     (solve' m b)
                Multiply a b -> (solve' m a) *     (solve' m b)
                Divide a b ->   (solve' m a) `div` (solve' m b)
    in result

partA :: Input -> String
partA input = show $ solve' input "root"

getTwoSides :: Operation -> (String, String)
getTwoSides (Plus a b) = (a, b)
getTwoSides (Minus a b) = (a, b)
getTwoSides (Multiply a b) = (a, b)
getTwoSides (Divide a b) = (a, b)
getTwoSides (Number _) = error "Not a binary operation"

test :: Map.Map String Operation -> Int -> Bool
test m human = let
    newMap = Map.insert "humn" (Number human) m
    (a, b) = getTwoSides $ m Map.! "root"
    leftSide = solve' newMap a
    rightSide = solve' newMap b
    in leftSide == rightSide

findEqualityPoint :: Map.Map String Operation -> Int
findEqualityPoint m = fromJust $ find (test m) [1..]

partB :: Input -> String
partB input = show $ findEqualityPoint input

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
