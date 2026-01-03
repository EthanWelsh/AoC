module Year2021.Day03 (solve) where

import Data.List (partition, transpose)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)

type Bit = Char

type Byte = String

type Input = [Byte]

inputParser :: Parser Input
inputParser = (some (char '0' <|> char '1')) `sepEndBy` eol

byteToInt :: Byte -> Integer
byteToInt bits =
  let powersOfTwo :: [Integer]
      powersOfTwo = [2 ^ x | x <- [0 ..] :: [Integer]]
      zipped = zip (reverse bits) powersOfTwo :: [(Bit, Integer)]
      onlyOn = filter ((== '1') . fst) zipped :: [(Bit, Integer)]
      totals = map snd onlyOn :: [Integer]
   in sum totals

keepWhereBitInPosEquals :: [Byte] -> Int -> Bit -> [Byte]
keepWhereBitInPosEquals bs pos target =
  let getBit b = b !! pos
      bitMatches b = (getBit b) == target
   in filter bitMatches bs

getCounts :: [Byte] -> Int -> (Int, Int)
getCounts bs pos =
  let zeroCount = length $ keepWhereBitInPosEquals bs pos '0'
      oneCount = length $ keepWhereBitInPosEquals bs pos '1'
   in (zeroCount, oneCount)

pickBigger :: ([Bit], [Bit]) -> Bit
pickBigger (zeros, ones) = if (length zeros) > (length ones) then '0' else '1'

pickSmaller :: ([Bit], [Bit]) -> Bit
pickSmaller (zeros, ones) = if (length zeros) < (length ones) then '0' else '1'

pickGamma :: [Byte] -> Byte
pickGamma bs =
  let transposed = transpose bs :: [Byte]
      partitioned = map (partition (== '0')) transposed :: [([Bit], [Bit])]
   in map pickBigger partitioned

pickEpsilon :: [Byte] -> Byte
pickEpsilon bs =
  let transposed = transpose bs :: [Byte]
      partitioned = map (partition (== '0')) transposed :: [([Bit], [Bit])]
   in map pickSmaller partitioned

partA :: Input -> String
partA bs = show $ byteToInt (pickGamma bs) * byteToInt (pickEpsilon bs)

pickOxygen :: [Byte] -> Byte
pickOxygen bs = pickOxygenHelp bs 0

pickOxygenHelp :: [Byte] -> Int -> Byte
pickOxygenHelp [b] _ = b
pickOxygenHelp bs pos =
  let counts = getCounts bs pos
      target = if snd counts >= fst counts then '1' else '0'
      filtered = keepWhereBitInPosEquals bs pos target
   in pickOxygenHelp filtered (pos + 1)

pickCarbon :: [Byte] -> Byte
pickCarbon bs = pickCarbonHelp bs 0

pickCarbonHelp :: [Byte] -> Int -> Byte
pickCarbonHelp [b] _ = b
pickCarbonHelp bs pos =
  let counts = getCounts bs pos
      target = if fst counts <= snd counts then '0' else '1'
      filtered = keepWhereBitInPosEquals bs pos target
   in pickCarbonHelp filtered (pos + 1)

partB :: Input -> String
partB bs = show $ byteToInt (pickCarbon bs) * byteToInt (pickOxygen bs)

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
