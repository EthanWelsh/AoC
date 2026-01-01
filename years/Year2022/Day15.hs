{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Year2022.Day15 (solve) where

import           Control.Monad              (void)
import           Data.Function              (on)
import qualified "unordered-containers" Data.HashSet as HashSet
import           Data.List
import           Data.Maybe                 (fromJust, mapMaybe)
import           Data.Range
import           Parsers                    (Parser, signedInteger)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Matrix                     (Point)
import           Util                       (range)

type Points = HashSet.HashSet Point
type Input = [(Point, Point)]

lineParser :: Parser (Point, Point)
lineParser = do
    void $ string "Sensor at x="
    sensorX <- signedInteger
    void $ string ", y="
    sensorY <- signedInteger
    void $ string ": closest beacon is at x="
    beaconX <- signedInteger
    void $ string ", y="
    beaconY <- signedInteger

    -- Convert to (r,c) instead of (x,y)
    let sensor = (sensorY, sensorX)
    let beacon = (beaconY, beaconX)

    return (sensor, beacon)

inputParser :: Parser Input
inputParser = lineParser `sepEndBy` eol

getRow :: Point -> Int
getRow (r, _) = r

getCol :: Point -> Int
getCol (_, c) = c

getSignal :: (Point, Point) -> Point
getSignal (s, _) = s

getBeacon :: (Point, Point) -> Point
getBeacon (_, b) = b

manhattanDistance :: Point -> Point -> Int
manhattanDistance (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

coversRows :: Point -> Point -> Range Int
coversRows (r1, _) (r2, _) = r1 +=+ r2

rangeSize :: Range Int -> Int
rangeSize (SpanRange (Bound a Inclusive) (Bound b Inclusive)) = abs (a-b) + 1
rangeSize (SpanRange (Bound a Exclusive) (Bound b Exclusive)) = abs (a-b) - 2
rangeSize (SpanRange (Bound a Inclusive) (Bound b Exclusive)) = abs (a-b)
rangeSize (SpanRange (Bound a Exclusive) (Bound b Inclusive)) = abs (a-b)
rangeSize _ = 0

shrinkInBothDirectionsBy :: Range Int -> Int -> Maybe (Range Int)
shrinkInBothDirectionsBy (SpanRange (Bound aval Inclusive) (Bound bval Inclusive)) x = let
    newA = aval + x
    newB = bval - x
    in if newA >= newB then Nothing else Just (SpanRange (Bound (aval+x) Inclusive) (Bound (bval-x) Inclusive))
shrinkInBothDirectionsBy _ _ = Nothing

coversTargetRow :: Int -> (Point, Point) -> Bool
coversTargetRow target (a, b) = inRange (coversRows a b) target

projectOutwards :: Int -> (Point, Point) -> Maybe (Range Int)
projectOutwards target (s@(r1, c1), b@(r2, c2)) = let
    distance = manhattanDistance s b
    verticalDistance = abs (r1 - target)
    coveredAtOrigin = (c1 - distance) +=+ (c1 + distance)
    in shrinkInBothDirectionsBy coveredAtOrigin verticalDistance

impossibleBeaconLocationsOnRow :: [(Point, Point)] -> Int -> [Range Int]
impossibleBeaconLocationsOnRow input targetRow = let
    colsCoveredOnTargetRow = mapMaybe (projectOutwards targetRow) input             :: [Range Int]
    joined = joinRanges colsCoveredOnTargetRow                                      :: [Range Int]
    beacons = map getBeacon input                                                 :: [Point]
    beaconsOnTargetRow = filter ( (==targetRow) . getRow) beacons                   :: [Point]
    beaconColsOnTargetRow = map (SingletonRange . getCol) beaconsOnTargetRow        :: [Range Int]
    rowsWithNoBeacons = difference joined beaconColsOnTargetRow                     :: [Range Int]
    in rowsWithNoBeacons

partA :: Input -> String
partA input = let
    targetRow = 2000000
    rowsWithNoBeacons = impossibleBeaconLocationsOnRow input targetRow
    in show $ sum $ map rangeSize rowsWithNoBeacons

rangeToList :: Range Int -> [Int]
rangeToList (SingletonRange a) = [a]
rangeToList (SpanRange (Bound a Inclusive) (Bound b Inclusive)) = [a     .. b]
rangeToList (SpanRange (Bound a Inclusive) (Bound b Exclusive)) = [a     .. (b-1)]
rangeToList (SpanRange (Bound a Exclusive) (Bound b Inclusive)) = [(a+1) .. (b)]
rangeToList (SpanRange (Bound a Exclusive) (Bound b Exclusive)) = [(a+1) .. (b-1)]
rangeToList _ = []

possibleBeaconLocationsOnRow :: [(Point, Point)] -> Range Int -> Int -> [Point]
possibleBeaconLocationsOnRow input inBounds targetRow = let
    impossibleRanges = impossibleBeaconLocationsOnRow input targetRow
    possibleRanges = difference [inBounds] impossibleRanges
    possibleCols = concatMap rangeToList possibleRanges
    in map (targetRow,) possibleCols

possibleLocations :: [(Point, Point)] -> Int -> Int -> [Point]
possibleLocations input lower upper = let
    inBounds = lower +=+ upper
    allCols = [lower..upper]
    in concatMap (possibleBeaconLocationsOnRow input inBounds) allCols

partB :: Input -> String
partB input = let
    locations = HashSet.fromList $ possibleLocations input 0 4000000
    beacons = HashSet.fromList $ map getBeacon input
    beaconLocation = head $ HashSet.toList $ HashSet.difference locations beacons
    (r, c) = beaconLocation
    in show $ (c*4000000) + r

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