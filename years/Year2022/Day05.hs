module Year2022.Day05 (solve) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.List (foldl', head, transpose)
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2022/input/sample/Day05.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- "CMZ"
-- >>> partB parsedExample
-- "MCD"

------------ TYPES ------------
type Stack = [Char]

type Instruction = (Int, Int, Int)

type Input = (Vector Stack, [Instruction])

------------ PARSER ------------
instructionParser :: Parser Instruction
instructionParser = do
  void $ string "move "
  quantity <- L.decimal
  void $ string " from "
  from <- L.decimal
  void $ string " to "
  to <- L.decimal
  return (quantity, from - 1, to - 1)

crateParser :: Parser (Maybe Char)
crateParser =
  (string "   " >> return Nothing)
    <|> (char '[' *> (Just <$> letterChar) <* char ']')

rowParser :: Parser [Maybe Char]
rowParser = crateParser `sepBy` char ' '

stacksParser :: Parser (Vector Stack)
stacksParser = do
  rows <- manyTill (rowParser <* eol) (try (void $ hspace >> digitChar))
  void $ takeWhileP Nothing (/= '\n') >> eol
  void eol
  return $ Vec.fromList . map catMaybes . transpose $ rows

inputParser :: Parser Input
inputParser = do
  stacks <- stacksParser
  instructions <- instructionParser `sepEndBy` eol
  return (stacks, instructions)

------------ PART A ------------
push :: a -> [a] -> [a]
push = (:)

pop :: [a] -> (a, [a])
pop (x : xs) = (x, xs)
pop _ = error "Cannot pop from empty list"

popThenPush :: Int -> Int -> Vector Stack -> Vector Stack
popThenPush fromIndex toIndex stacks =
  let from = stacks Vec.! fromIndex
      to = stacks Vec.! toIndex
      (x, popped) = pop from
      pushed = push x to
   in stacks Vec.// [(fromIndex, popped), (toIndex, pushed)]

getNormalizedInstructions :: [Instruction] -> [(Int, Int)]
getNormalizedInstructions ins =
  let norm (quantity, from, to) = replicate quantity (from, to)
   in concatMap norm ins

run :: [(Int, Int)] -> Vector Stack -> Vector Stack
run ins stacks = foldl' (\st (from, to) -> popThenPush from to st) stacks ins

partA :: Input -> String
partA (stacks, ins) =
  let normalized = getNormalizedInstructions ins
      result = run normalized stacks
   in Vec.toList $ Vec.map head result

------------ PART B ------------
pushMultiple :: [Char] -> [Char] -> [Char]
pushMultiple pushes stack = pushes ++ stack

popMultiple :: Int -> [Char] -> ([Char], [Char])
popMultiple count stack = (take count stack, drop count stack)

popThenPushB :: Int -> Int -> Int -> Vector Stack -> Vector Stack
popThenPushB quantity fromIndex toIndex stacks =
  let from = stacks Vec.! fromIndex
      to = stacks Vec.! toIndex
      (xs, popped) = popMultiple quantity from
      pushed = pushMultiple xs to
   in stacks Vec.// [(fromIndex, popped), (toIndex, pushed)]

runB :: [Instruction] -> Vector Stack -> Vector Stack
runB ins stacks = foldl' (\st (q, f, t) -> popThenPushB q f t st) stacks ins

partB :: Input -> String
partB (stacks, ins) =
  let result = runB ins stacks
   in Vec.toList $ Vec.map head result

------------ SOLVE ------------
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