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

import Parsers (Parser)
import Text.Megaparsec (parse, errorBundlePretty)

-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ readFile "years/Year$YEAR/input/sample/Day$DAY_PADDED.txt"
-- >>> let Right parsedExample = parse parseInput "" example

type Input = String

parseInput :: Parser Input
parseInput = error "TODO"

-- |
-- >>> part1 parsedExample
-- Part 1: 0
part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print input

-- |
-- >>> part2 parsedExample
-- Part 2: 0
part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
EOL

  # Add to cabal file
  line_to_add="                     , Year$YEAR.Day$DAY_PADDED"
  last_line=$(grep -n "Year$YEAR" aoc.cabal | tail -n 1 | cut -d: -f1)
  if [ -z "$last_line" ]; then
    last_line=$(grep -n "other-modules" aoc.cabal | head -n 1 | cut -d: -f1)
  fi
  awk -v line_num="$last_line" -v text_to_add="$line_to_add" 'NR == line_num {print; print text_to_add} NR != line_num {print}' aoc.cabal > aoc.cabal.tmp && mv aoc.cabal.tmp aoc.cabal

  # Add to Main.hs
  import_to_add="import qualified Year$YEAR.Day$DAY_PADDED"
  last_import_line=$(grep -n "import qualified Year$YEAR" src/Main.hs | tail -n 1 | cut -d: -f1)
  if [ -z "$last_import_line" ]; then
    last_import_line=$(grep -n "import qualified" src/Main.hs | tail -n 1 | cut -d: -f1)
  fi
  awk -v line_num="$last_import_line" -v text_to_add="$import_to_add" 'NR == line_num {print; print text_to_add} NR != line_num {print}' src/Main.hs > src/Main.hs.tmp && mv src/Main.hs.tmp src/Main.hs

  solver_to_add="    , Year$YEAR.Day$DAY_PADDED.solve"
  last_solver_line=$(grep -n "Year$YEAR.*.solve" src/Main.hs | tail -n 1 | cut -d: -f1)
  if [ -z "$last_solver_line" ]; then
      last_solver_line=$(grep -n "solvers$YEAR" src/Main.hs | head -n 1 | cut -d: -f1)
      # Add an extra comma to the previous line
      prev_line_num=$(($last_solver_line + 1))
      sed -i '' "$prev_line_num s/$/,/" src/Main.hs
  fi
  awk -v line_num="$last_solver_line" -v text_to_add="$solver_to_add" 'NR == line_num {print; print text_to_add} NR != line_num {print}' src/Main.hs > src/Main.hs.tmp && mv src/Main.hs.tmp src/Main.hs


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
