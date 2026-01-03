#!/bin/zsh

set -e

YEAR=$1
DAY=$2
LANGUAGE=$3

if [ -z "$YEAR" ] || [ -z "$DAY" ] || [ -z "$LANGUAGE" ]; then
  echo "Usage: $0 <year> <day> <haskell|rust>"
  exit 1
fi

DAY_PADDED=$(printf "%02d" $DAY)
DIR="years/Year$YEAR"
FILE_HS="$DIR/Day$DAY_PADDED.hs"
FILE_RS="$DIR/Day$DAY_PADDED.rs"

mkdir -p "$DIR"
mkdir -p "$DIR/input/sample"
mkdir -p "$DIR/input/real"
touch "$DIR/input/sample/Day$DAY_PADDED.txt"
touch "$DIR/input/real/Day$DAY_PADDED.txt"

if [ "$LANGUAGE" = "haskell" ]; then
  cat > "$FILE_HS" << EOL
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
  cmd=$(printf '/, Year%s/a\
, Year%s.Day%s' "$YEAR" "$YEAR" "$DAY_PADDED")
  sed -i '' "$cmd" aoc.cabal

  # Add to Main.hs
  cmd1=$(printf '/import qualified Year%s/a\
import qualified Year%s.Day%s' "$YEAR" "$YEAR" "$DAY_PADDED")
  sed -i '' "$cmd1" src/Main.hs
  cmd2=$(printf '/, Year%s.*.solve/a\
, Year%s.Day%s.solve' "$YEAR" "$YEAR" "$DAY_PADDED")
  sed -i '' "$cmd2" src/Main.hs

  echo "Created $FILE_HS"

elif [ "$LANGUAGE" = "rust" ]; then
  cat > "$FILE_RS" << EOL
advent_of_code::solution!($DAY);

pub fn part_one(input: &str) -> Option<u32> {
    None
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
EOL

  # Add to Cargo.toml
  CARGO_FILE="$DIR/Cargo.toml"
  if [ -f "$CARGO_FILE" ]; then
    echo "" >> "$CARGO_FILE"
    echo "[[bin]]" >> "$CARGO_FILE"
    echo "name = \"day$DAY_PADDED\"" >> "$CARGO_FILE"
    echo "path = \"Day$DAY_PADDED.rs\"" >> "$CARGO_FILE"
  else
    echo "Cargo.toml not found in $DIR"
  fi

  echo "Created $FILE_RS"

else
  echo "Invalid language: $LANGUAGE. Please use 'haskell' or 'rust'."
  exit 1
fi