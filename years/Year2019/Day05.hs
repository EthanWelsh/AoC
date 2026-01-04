-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year2019/input/sample/Day05.txt"
-- >>> let Right parsedExample = parse parseInput "" example
-- >>> let runWithInput i = getOutput $ runProgram (parsedExample {getInput = [i]})
-- >>> runWithInput 7
-- [999]
-- >>> runWithInput 8
-- [1000]
-- >>> runWithInput 9
-- [1001]

module Year2019.Day05 (solve) where

import Intcode
import Parsers (Parser, signedInteger)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char (char)

type Input = Machine

parseInput :: Parser Input
parseInput = do
  m <- signedInteger `sepBy` char ','
  return $ machineFromList m

part1 :: Input -> String
part1 input = show $ getOutput $ runProgram (input {getInput = [1]})

part2 :: Input -> String
part2 input = show $ getOutput $ runProgram (input {getInput = [5]})

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      putStrLn $ part1 input
      putStr "Part 2: "
      putStrLn $ part2 input
