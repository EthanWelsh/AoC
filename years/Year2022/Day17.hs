module Year2022.Day17 (solve) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.List
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Parsers                    (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Direction = L | R | D deriving (Eq, Show)
type Point = (Int, Int)

data Shape = HorizontalLine | Plus | Angle | VerticalLine | Block deriving (Eq, Show)

type Input = [Direction]
type Points = Set.Set Point

directionParser :: Parser Direction
directionParser = do
    c <- (char '<') <|> (char '>')
    return $ if c == '<' then L else R

inputParser :: Parser Input
inputParser = some directionParser

getRow :: Point -> Int
getRow p = fst p

getCol :: Point -> Int
getCol p = snd p

spawnShapeFromPoint :: Point -> Shape -> Points
spawnShapeFromPoint (r, c) HorizontalLine = Set.fromList [(r, c), (r, c + 1), (r, c + 2), (r, c + 3)]
spawnShapeFromPoint (r, c) VerticalLine = Set.fromList [(r, c), (r + 1 , c), (r + 2, c), (r + 3, c)]
spawnShapeFromPoint (r, c) Plus = Set.fromList [(r, c + 1), (r + 1, c), (r + 1, c + 1), (r + 1, c + 2), (r + 2, c + 1)]
spawnShapeFromPoint (r, c) Angle = Set.fromList [(r, c), (r, c + 1), (r, c + 2), (r + 1, c + 2), (r + 2, c + 2)]
spawnShapeFromPoint (r, c) Block = Set.fromList [(r, c), (r, c + 1), (r + 1, c), (r + 1, c + 1)]

spawnShape :: Points -> Shape -> Points
spawnShape points shape = let
    leftEdge = 2
    bottomEdge = getHighestPoint points + 4
    in spawnShapeFromPoint (bottomEdge, leftEdge) shape

rotatingShapes :: [Shape]
rotatingShapes = cycle [HorizontalLine, Plus, Angle, VerticalLine, Block]

rotatingDirections :: [Direction] -> [Direction]
rotatingDirections directions = intersperse D (cycle directions)

collision :: Points -> Points -> Bool
collision a b = not $ Set.null (Set.intersection a b)

nudge :: Direction -> Points -> Points
nudge L points = Set.map (\(r, c) -> (r, c - 1)) points
nudge R points = Set.map (\(r, c) -> (r, c + 1)) points
nudge D points = Set.map (\(r, c) -> (r - 1, c)) points

getHighestPoint :: Points -> Int
getHighestPoint points = let
    rows = Set.map getRow points
    in if Set.null rows then -1 else maximum rows

canMove :: Points -> Points -> Direction -> Bool
canMove points shape dir = not (cantMove points shape dir)

cantMove :: Points -> Points -> Direction -> Bool
cantMove points shape dir = let
    nudged = nudge dir shape
    rows = Set.map getRow nudged
    cols = Set.map getCol nudged
    minRow = Set.findMin rows
    minCol = Set.findMin cols
    maxCol = Set.findMax cols
    collidesWithOther = collision points nudged
    collidesWithFloor = minRow < 0
    collidesWithWall = minCol < 0 || maxCol >= 7
    in collidesWithOther || collidesWithFloor || collidesWithWall

dropShape :: (Points, [Direction], [Shape]) -> (Points, [Direction], [Shape])
dropShape (points, dirs, shape:shapes) = let
    shapePoints = spawnShape points shape
    (newPoints, newDirs) = settleShape points shapePoints dirs
    in (newPoints, newDirs, shapes)

settleShape :: Points -> Points -> [Direction] -> (Points, [Direction])
settleShape points shape (D:dirs)
    | cantMove points shape D = (Set.union points shape, dirs)
settleShape points shape (dir:dirs)
    | canMove points shape dir = settleShape points (nudge dir shape) dirs
    | cantMove points shape dir = settleShape points shape dirs
    | otherwise = error "Should not happen"

infiniteSimulation :: [Direction] -> [(Points, [Direction], [Shape])]
infiniteSimulation dirs = iterate dropShape (Set.empty, dirs, rotatingShapes)

partA :: Input -> String
partA input = do
    let dirs = (rotatingDirections input)
    let (points, _, _) = infiniteSimulation dirs !! 2022
    show $ (getHighestPoint points) + 1

partB :: Input -> String
partB input = do
    let dirs = (rotatingDirections input)
    let (points, _, _) = infiniteSimulation dirs !! 10000 --1000000000000
    show $ (getHighestPoint points) + 1

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