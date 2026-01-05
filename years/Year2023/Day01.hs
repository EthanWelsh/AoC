module Year2023.Day01 (solve) where

import Data.List (findIndex, isPrefixOf)

-- $setup
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example1 = lines $ unsafePerformIO $ readFile "years/Year2023/input/sample/Day01_part1.txt"
-- >>> let example2 = lines $ unsafePerformIO $ readFile "years/Year2023/input/sample/Day01_part2.txt"
-- >>> partA example1
-- 142
-- >>> partB example2
-- 281

type Input = [String]

calibrationValue :: [String] -> (String -> Int) -> String -> Int
calibrationValue ns toNum s =
  let left = toNum $ getFirst s ns
      right = toNum $ getLast s ns
   in left * 10 + right

getFirst :: String -> [String] -> String
getFirst = firstPrefix

getLast :: String -> [String] -> String
getLast s ns = reverse $ getFirst (reverse s) (map reverse ns)

firstPrefix :: String -> [String] -> String
firstPrefix s@(_ : xs) ps = case findIndex (`isPrefixOf` s) ps of
  Nothing -> firstPrefix xs ps
  (Just i) -> ps !! i
firstPrefix [] _ = error "unexpected"

toNumber :: String -> Int
toNumber "one" = 1
toNumber "two" = 2
toNumber "three" = 3
toNumber "four" = 4
toNumber "five" = 5
toNumber "six" = 6
toNumber "seven" = 7
toNumber "eight" = 8
toNumber "nine" = 9
toNumber d = read d

digits :: [String]
digits = map show ([1 .. 9] :: [Int])

digitsAndWords :: [String]
digitsAndWords = digits ++ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

partA :: Input -> Int
partA = sum . map (calibrationValue digits read)

partB :: Input -> Int
partB = sum . map (calibrationValue digitsAndWords toNumber)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  let input = lines contents
  putStrLn $ "Part 1: " ++ show (partA input)
  putStrLn $ "Part 2: " ++ show (partB input)