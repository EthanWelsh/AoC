{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Year2025.Day09 (solve) where

import Data.List (sortBy, tails)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)
import qualified Text.Megaparsec.Char.Lexer as L
import "unordered-containers" Data.HashSet (HashSet)
import qualified "unordered-containers" Data.HashSet as HashSet

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2025/input/sample/Day09.txt"
-- >>> let Right parsedExample = parse parseInput "" example

type Point = (Int, Int)

type Input = [Point]

data Polygon = Polygon {boundary :: HashSet Point, vertices :: [Point]} deriving (Show, Eq)

parsePoint :: Parser Point
parsePoint = (,) <$> L.decimal <* char ',' <*> L.decimal

parseInput :: Parser Input
parseInput = parsePoint `sepEndBy` eol

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

-- |
-- >>> part1 parsedExample
-- Part 1: 50
part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let pairs = allPairs input
  let sorted = sortBy (flip \(a, b) (c, d) -> compare (area a b) (area c d)) pairs
  let topArea = uncurry area (head sorted)
  print topArea

getPointsInLine :: Point -> Point -> [Point]
getPointsInLine (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y) | y <- range y1 y2]
  | y1 == y2 = [(x, y1) | x <- range x1 x2]
  | otherwise = error "Not a straight line"
  where
    range a b
      | a <= b = [a .. b]
      | otherwise = [b .. a]

getPolygon :: [Point] -> Polygon
getPolygon pts = Polygon (HashSet.fromList $ concatMap (uncurry getPointsInLine) edges) pts
  where
    edges = zip pts (tail (cycle pts))

rayCast :: (Double, Double) -> [Point] -> Bool
rayCast (px, py) vs = odd intersections
  where
    intersections = length $ filter intersects (zip vs (tail (cycle vs)))

    intersects ((x1, y1), (x2, y2)) =
      let v1y = fromIntegral y1 :: Double
          v2y = fromIntegral y2 :: Double
          straddles = (v1y > py) /= (v2y > py)

          inverseSlope = fromIntegral (x2 - x1) / (v2y - v1y)
          intersectX = fromIntegral x1 + inverseSlope * (py - v1y)
       in straddles && px < intersectX

isPointInPolygon :: Point -> Polygon -> Bool
isPointInPolygon p@(px, py) (Polygon b vs)
  | HashSet.member p b = True
  | otherwise = rayCast (fromIntegral px, fromIntegral py) vs

isPointInPolygonDouble :: (Double, Double) -> Polygon -> Bool
isPointInPolygonDouble p (Polygon _ vs) = rayCast p vs

edgeCrossesRect :: (Point, Point) -> (Point, Point) -> Bool
edgeCrossesRect ((rx1, ry1), (rx2, ry2)) ((px1, py1), (px2, py2))
  | px1 == px2 = strictlyInside px1 rxX && intervalsOverlap pyY rxY -- Vertical edge
  | otherwise = strictlyInside py1 rxY && intervalsOverlap pxX rxX -- Horizontal edge
  where
    rxX = (min rx1 rx2, max rx1 rx2)
    rxY = (min ry1 ry2, max ry1 ry2)
    pxX = (min px1 px2, max px1 px2)
    pyY = (min py1 py2, max py1 py2)

    strictlyInside v (low, high) = v > low && v < high
    intervalsOverlap (a1, a2) (b1, b2) = max a1 b1 < min a2 b2

isRectangeInPolygon :: Polygon -> (Point, Point) -> Bool
isRectangeInPolygon polygon rect@((x1, y1), (x2, y2)) =
  cornersInside && centerInside && noEdgeCrossing
  where
    corners = [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]
    cornersInside = all (`isPointInPolygon` polygon) corners

    centerX = fromIntegral (x1 + x2) / 2.0
    centerY = fromIntegral (y1 + y2) / 2.0
    centerInside = isPointInPolygonDouble (centerX, centerY) polygon

    noEdgeCrossing = not $ any (edgeCrossesRect rect) (zip (vertices polygon) (tail (cycle (vertices polygon))))

-- |
-- >>> part2 parsedExample
-- Part 2: 24
part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let polygon = getPolygon input
  let pairs = allPairs input
  let rectangles = [(p1, p2) | (p1, p2) <- pairs, isRectangeInPolygon polygon (p1, p2)]
  print $ maximum $ map (uncurry area) rectangles

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
