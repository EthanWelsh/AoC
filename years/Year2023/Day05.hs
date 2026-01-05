module Year2023.Day05 (solve) where

import Control.Monad (void)
import Data.Function (on)
import Data.Interval
import qualified Data.IntervalSet as IntervalSet
import Data.List (find)
import Data.Maybe
import qualified Data.Set as Set
import List (pairsByTwo)
import Parsers (Parser, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (string)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2023/input/sample/Day05.txt"
-- >>> let Right parsedExample = parse parseInput "" example
-- >>> partA parsedExample
-- 35
-- >>> partB parsedExample
-- 46

data Range = Range
  {
    destination :: Int,
    source :: Int,
    len :: Int
  }
  deriving (Show, Eq)

instance Ord Range where
  compare = compare `on` source

type ResourceMap = Set.Set Range

data Almanac = Almanac [Int] [ResourceMap] deriving (Show)

parseRange :: Parser Range
parseRange = do
  d <- integer
  s <- integer
  l <- integer
  return (Range d s l)

parseResourceMap :: Parser (Set.Set Range)
parseResourceMap = do
  ranges <- some parseRange
  return $ Set.fromList ranges

parseInput :: Parser Almanac
parseInput = do
  void $ string "seeds: "
  seeds <- some integer
  void $ string "seed-to-soil map:\n"
  seedSoil <- parseResourceMap
  void $ string "soil-to-fertilizer map:\n"
  soilFertilizer <- parseResourceMap
  void $ string "fertilizer-to-water map:\n"
  fertilizerWater <- parseResourceMap
  void $ string "water-to-light map:\n"
  waterLight <- parseResourceMap
  void $ string "light-to-temperature map:\n"
  lightTemp <- parseResourceMap
  void $ string "temperature-to-humidity map:\n"
  tempHumidity <- parseResourceMap
  void $ string "humidity-to-location map:\n"
  humidityLocation <- parseResourceMap
  return
    (
      Almanac
        seeds
        [
          seedSoil,
          soilFertilizer,
          fertilizerWater,
          waterLight,
          lightTemp,
          tempHumidity,
          humidityLocation
        ]
    )

inRange :: Range -> Int -> Bool
inRange (Range _ s l) t = t >= s && t < (s + l)

mapFromRange :: Range -> Int -> Maybe Int
mapFromRange r@(Range d s _) t =
  if inRange r t
    then Just (d + (t - s))
    else Nothing

mapFromResourceMap :: ResourceMap -> Int -> Int
mapFromResourceMap rs t = fromMaybe t mpd
  where
    mpd = do
      lower <- Set.lookupLE (Range 0 t 0) rs
      mapped <- mapFromRange lower t
      return mapped

mapFromResourceMaps :: [ResourceMap] -> Int -> Int
mapFromResourceMaps maps t = foldl (flip mapFromResourceMap) t maps

createInterval :: (Int, Int) -> Interval Int
createInterval (s, l) = start <=..< end
  where
    start = Finite s
    end = Finite (s + l)

seedsAsRanges :: [Int] -> IntervalSet.IntervalSet Int
seedsAsRanges seeds =
  let ps = pairsByTwo seeds :: [(Int, Int)]
      intervals = map createInterval ps :: [Interval Int]
   in IntervalSet.fromList intervals

reverseRange :: Range -> Range
reverseRange (Range d s l) = Range s d l

reverseResourceMap :: ResourceMap -> ResourceMap
reverseResourceMap ranges =
  let l = Set.toList ranges :: [Range]
      r = map reverseRange l
   in Set.fromList r

reverseResourceMaps :: [ResourceMap] -> [ResourceMap]
reverseResourceMaps maps = reverse $ map reverseResourceMap maps

partA :: Almanac -> Int
partA (Almanac seeds maps) =
  let seedLocations = map (mapFromResourceMaps maps) seeds
   in minimum seedLocations

partB :: Almanac -> Int
partB (Almanac seeds maps) =
  let revd = reverseResourceMaps maps
      seedRanges = seedsAsRanges seeds
      ascending = [0 ..] :: [Int]
      locationToSeeds = zip ascending $ map (mapFromResourceMaps revd) ascending
      lowest = find (\(_, s) -> IntervalSet.member s seedRanges) locationToSeeds
   in (fst . fromJust) lowest

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right almanac -> do
      putStrLn $ "Part 1: " ++ show (partA almanac)
      putStrLn $ "Part 2: " ++ show (partB almanac)