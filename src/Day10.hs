{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Day10 (solve) where

import Text.Megaparsec (parse, errorBundlePretty, between, sepBy, many, (<|>))
import Utils.Parsers (Parser, sc, lexeme, symbol, integer)
import Text.Megaparsec.Char (char)
import Algorithm.Search
import Control.Lens (element, (%~), (&))
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (subsequences)
import Data.Function.Memoize (memoize)


data Light = On | Off deriving (Show, Eq, Ord)
newtype WiringSchematic = WiringSchematic [Int] deriving (Show, Eq, Ord)
newtype Joltage = Joltage [Int] deriving (Show, Eq, Ord)

data Machine = Machine
  { targetLights :: [Light]
  , schematics :: [WiringSchematic]
  , requirements :: Joltage
  } deriving (Show, Eq, Ord)

type Input = [Machine]

-- The manual describes one machine per line. 
-- Each line contains:
--   - a single indicator light diagram in [square brackets]
--   - one or more button wiring schematics in (parentheses)
--   - joltage requirements in {curly braces}.
-- Example lines:
-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
-- [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
-- [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
parseLights :: Parser [Light]
parseLights = do
  lightDiagram <- between (symbol "[") (symbol "]") (lexeme (many (char '#' <|> char '.')))
  return $ map (\c -> if c == '#' then On else Off) lightDiagram

parseSchematics :: Parser [WiringSchematic]
parseSchematics = do
  xs <- many (between (symbol "(") (symbol ")") (sepBy integer (symbol ",")))
  return $ map WiringSchematic xs

parseRequirements :: Parser Joltage
parseRequirements = do
  joltageReqs <- between (symbol "{") (symbol "}") (sepBy integer (symbol ","))
  return $ Joltage joltageReqs

machineParser :: Parser Machine
machineParser = Machine <$> parseLights <*> parseSchematics <*> parseRequirements

parseInput :: Parser Input
parseInput = sc >> many machineParser

toggleLights :: [Light] -> WiringSchematic -> [Light]
toggleLights ls (WiringSchematic indices) = foldl toggle ls indices
  where
    toggle :: [Light] -> Int -> [Light]
    toggle xs i = xs & element i %~ (\l -> if l == On then Off else On)

solveLights :: Machine -> Int
solveLights m = case shortestPath of
    Just (c, _) -> c
    Nothing -> error "No solution found"
  where
    shortestPath :: Maybe (Int, [[Light]])
    shortestPath = dijkstra neighbors cost isGoal initialState

    neighbors :: [Light] -> [[Light]]
    neighbors lights = map (toggleLights lights) (schematics m)

    cost :: [Light] -> [Light] -> Int
    cost _ _ = 1

    isGoal :: [Light] -> Bool
    isGoal lights = lights == targetLights m

    initialState :: [Light]
    initialState = replicate (length (targetLights m)) Off

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ sum $ map solveLights input

decreaseJoltage :: Joltage -> WiringSchematic -> Joltage
decreaseJoltage (Joltage ls) (WiringSchematic indices) = Joltage $ foldl decrease ls indices
  where
    decrease :: [Int] -> Int -> [Int]
    decrease xs i = xs & element i %~ subtract 1

solveJoltage :: Machine -> Int
solveJoltage m = fromMaybe (error "No solution found") (solve (let (Joltage r) = requirements m in r))
  where
    buttons = schematics m
    allSubsets = subsequences buttons

    solve :: [Int] -> Maybe Int
    solve = memoize solve'

    solve' :: [Int] -> Maybe Int
    solve' current
      | all (== 0) current = Just 0
      | any (< 0) current = Nothing
      | otherwise =
          let candidates = filter (isEvenAfter current) allSubsets
              results = map (processCandidate current) candidates
              validResults = catMaybes results
          in if null validResults then Nothing else Just (minimum validResults)

    isEvenAfter :: [Int] -> [WiringSchematic] -> Bool
    isEvenAfter start subset =
        let res = applySubset start subset
        in all even res

    applySubset :: [Int] -> [WiringSchematic] -> [Int]
    applySubset start subset =
        let (Joltage res) = foldl decreaseJoltage (Joltage start) subset
        in res

    processCandidate :: [Int] -> [WiringSchematic] -> Maybe Int
    processCandidate start subset =
        let reduced = applySubset start subset
        in if any (< 0) reduced
           then Nothing
           else
             let nextJoltage = map (`div` 2) reduced
             in case solve nextJoltage of
                  Nothing -> Nothing
                  Just v -> Just (length subset + 2 * v)

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print $ sum $ map solveJoltage input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
