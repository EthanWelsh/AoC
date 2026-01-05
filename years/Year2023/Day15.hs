module Year2023.Day15 (solve) where

import Control.Lens
import Control.Monad (void)
import Data.Char
import Data.List (findIndices)
import Parsers (Parser, integer)
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char (alphaNumChar, char)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2023/input/sample/Day15.txt"
-- >>> let Right parsedExample1 = parse parseInputPt1 "" example
-- >>> let Right parsedExample2 = parse parseInputPt2 "" example
-- >>> partA parsedExample1
-- 1320
-- >>> partB parsedExample2
-- 145

data Instruction = Add String Int | Remove String deriving (Show)

type Box = [(String, Int)]

type BoxMap = [Box]

parseInputPt1 :: Parser [String]
parseInputPt1 = many (choice [alphaNumChar, char '=', char '-']) `sepBy` char ','

parseInputPt2 :: Parser [Instruction]
parseInputPt2 = parseInstruction `sepBy` char ','

parseInstruction :: Parser Instruction
parseInstruction = try $ choice [try parseAdd, try parseRemove]

parseAdd :: Parser Instruction
parseAdd = do
  label <- many alphaNumChar
  void $ char '='
  n <- integer
  return (Add label n)

parseRemove :: Parser Instruction
parseRemove = do
  label <- many alphaNumChar
  void $ char '-'
  return (Remove label)

partA :: [String] -> Int
partA = sum . map hash

emptyBoxMap :: BoxMap
emptyBoxMap = [[] | _ <- [0 .. 255] :: [Int]]

setBox :: BoxMap -> Int -> Box -> BoxMap
setBox bs i b = (element i .~ b) bs

addToBox :: Box -> String -> Int -> Box
addToBox b l v =
  let matchingBoxes = findIndices (\(x, _) -> x == l) b
      withNewVal = (l, v)
      containsLabel = not $ null matchingBoxes
   in if containsLabel then (element (head matchingBoxes) .~ withNewVal) b else b ++ [(l, v)]

removeFromBox :: Box -> String -> Box
removeFromBox b l = filter (\(x, _) -> x /= l) b

labelFromInstruction :: Instruction -> Int
labelFromInstruction (Add l _) = hash l
labelFromInstruction (Remove l) = hash l

followInstruction :: BoxMap -> Instruction -> BoxMap
followInstruction bs instruction =
  let i = labelFromInstruction instruction
      oldBox = bs !! i
   in case instruction of
        (Add label v) -> setBox bs i (addToBox oldBox label v)
        (Remove label) -> setBox bs i (removeFromBox oldBox label)

followInstructions :: [Instruction] -> BoxMap
followInstructions = foldl followInstruction emptyBoxMap

getFocusPower :: Box -> Int -> Int
getFocusPower b idx =
  let boxNum = idx + 1
      lensNums = zipWith (\i (_, v) -> i * v) [1 ..] b
   in if null b then 0 else boxNum * sum lensNums

partB :: [Instruction] -> Int
partB input =
  let bm = followInstructions input
      boxPowers = zipWith getFocusPower bm [0 ..]
   in sum boxPowers

hash :: String -> Int
hash = foldl helper 0
  where
    helper current c = ((current + ord c) * 17) `rem` 256

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInputPt1 filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStrLn $ "Part 1: " ++ show (partA input)
  case parse parseInputPt2 filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStrLn $ "Part 2: " ++ show (partB input)