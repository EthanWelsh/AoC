module Year2025.Day12 (solve) where

import GHC.Err (errorWithoutStackTrace)
import Maze (Maze, findPoints, height, mazeFromDimensions, mazeFromList, width)
import Parsers (Parser, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> let example = "0:\n###\n##.\n##.\n\n1:\n###\n##.\n.##\n\n2:\n.##\n###\n##.\n\n3:\n##.\n###\n##.\n\n4:\n###\n#..\n###\n\n5:\n###\n.#.\n###\n\n4x4: 0 0 0 0 2 0\n12x5: 1 0 1 0 2 2\n12x5: 1 0 1 0 3 2"
-- >>> let Right parsedExample = parse parseInput "" example

type Grid = Maze Char

type Gift = Grid

newtype Tree = Tree (Grid, [Int]) deriving (Show)

type Input = ([Gift], [Tree])

giftParser :: Parser Grid
giftParser = try $ do
  _ <- integer
  _ <- string ":\n"
  chars <- some (choice [char '#', char '.']) `sepEndBy` newline
  return $ mazeFromList chars

treeParser :: Parser Tree
treeParser = do
  w <- decimal
  _ <- char 'x'
  h <- decimal
  _ <- string ": "
  nums <- decimal `sepBy` char ' '
  return $ Tree (mazeFromDimensions w h '.', nums)

parseInput :: Parser Input
parseInput = do
  gifts <- giftParser `sepEndBy` newline
  _ <- many newline
  trees <- treeParser `sepEndBy` newline
  return (gifts, trees)

giftVolume :: Gift -> Int
giftVolume g = length $ findPoints g (== '#')

isFitImpossible :: [Gift] -> Tree -> Bool
isFitImpossible gifts (Tree (grid, giftCounts)) = 
  let giftVolumes = map giftVolume gifts
      totalGiftVolume = sum $ zipWith (*) giftVolumes giftCounts
      totalAvailableVolume = width grid * height grid
   in totalGiftVolume > totalAvailableVolume

isFitInevitable :: Tree -> Bool
isFitInevitable (Tree (grid, giftCounts)) = 
  let widthInGifts = width grid `div` 3 :: Int
      heightInGifts = height grid `div` 3 :: Int
      totalGiftsThatCanFit = widthInGifts * heightInGifts
      totalGiftsToPlace = sum giftCounts
   in totalGiftsToPlace <= totalGiftsThatCanFit

treesWillFit :: [Gift] -> Tree -> Bool
treesWillFit gifts tree = case (isFitImpossible gifts tree, isFitInevitable tree) of
  (True, True) -> errorWithoutStackTrace "Contradiction"
  (True, _) -> False
  (_, True) -> True
  (False, False) -> errorWithoutStackTrace "Input has complicated cases not handled"

-- |
-- >>> part1 parsedExample
-- Part 1: *** Exception: Input has complicated cases not handled
part1 :: Input -> IO ()
part1 (gifts, trees) = do
  putStr "Part 1: "
  print $ length $ filter (treesWillFit gifts) trees

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
