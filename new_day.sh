#!/bin/bash

set -e

YEAR=$1
DAY=$2

if [ -z "$YEAR" ] || [ -z "$DAY" ]; then
  echo "Usage: $0 <year> <day>"
  exit 1
fi

DAY_PADDED=$(printf "%02d" $DAY)
DIR="years/Year$YEAR"
FILE="$DIR/Day$DAY_PADDED.hs"

mkdir -p "$DIR"
mkdir -p "$DIR/input/sample"
mkdir -p "$DIR/input/real"
touch "$DIR/input/sample/Day$DAY_PADDED.txt"
touch "$DIR/input/real/Day$DAY_PADDED.txt"

cat > "$FILE" << EOL
module Year$YEAR.Day$DAY_PADDED (solve) where

--import           Parsers   (Parser)
--import           Text.Megaparsec
--import Control.Monad (void)
--import Text.Megaparsec.Char (string, char, newline)

type Input = String

--parseInput :: Parser Input
--parseInput = error "TODO"

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print input

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  part1 contents
  part2 contents
  --case parse parseInput filePath contents of
  --        Left eb -> putStr (errorBundlePretty eb)
  --        Right input -> do
  --          part1 input
  --          part2 input
EOL

# Add to cabal file
sed -i '' -e "/, Year$YEAR/ s/$/,/" aoc.cabal
sed -i '' -e "/, Year$YEAR/a\
, Year$YEAR.Day$DAY_PADDED
" aoc.cabal

# Add to Main.hs
sed -i '' -e "/import qualified Year$YEAR/ s/$/\nimport qualified Year$YEAR.Day$DAY_PADDED/" src/Main.hs
sed -i '' -e "/, Year$YEAR.*.solve/ s/$/,/" src/Main.hs
sed -i '' -e "/, Year$YEAR.*.solve/a\
, Year$YEAR.Day$DAY_PADDED.solve
" src/Main.hs


echo "Created $FILE"