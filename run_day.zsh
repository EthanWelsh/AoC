#!/bin/zsh

# Check for correct number of arguments
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <year> <day> <sample|real>"
    exit 1
fi

YEAR=$1
DAY=$2
SAMPLE_OR_REAL=$3

DAY_STR=$(printf "%02d" "${DAY}")

# Determine language based on file extension
# Check for Haskell solution first
echo "Checking for Haskell: years/Year${YEAR}/Day${DAY_STR}.hs"
if [ -f "years/Year${YEAR}/Day${DAY_STR}.hs" ]; then
    echo "Running Haskell solution for Day ${DAY_STR}, Year ${YEAR} (${SAMPLE_OR_REAL} input)..."
    stack run aoc "${YEAR}" "${DAY}" "${SAMPLE_OR_REAL}"
elif [ -f "years/Year${YEAR}/Day${DAY_STR}.rs" ]; then
    echo "Running Rust solution for Day ${DAY_STR}, Year ${YEAR} (${SAMPLE_OR_REAL} input)..."
    
    # Construct input file path
    INPUT_DIR="years/Year${YEAR}/input/${SAMPLE_OR_REAL}"
    INPUT_FILE="${INPUT_DIR}/Day${DAY_STR}.txt"

    # Create input directory if it doesn't exist
    mkdir -p "${INPUT_DIR}"

    # Download input file if it doesn't exist
    if [ ! -f "${INPUT_FILE}" ]; then
        echo "Downloading input for Day ${DAY_STR}, Year ${YEAR}..."
        curl "https://adventofcode.com/${YEAR}/day/${DAY}/input" \
            --cookie "session=53616c7465645f5f5720e144a5f544af0407f2ea67ec8290daddf18a903b4914e032fe474b3b16732b897c8189f79cc50db8a97663e5ebd9b2eaeff1751f9d32" \
            --output "${INPUT_FILE}"
        if [ $? -ne 0 ]; then
            echo "Error downloading input file."
            exit 1
        fi
    fi

    # Run Rust solution
    (cd "years/Year${YEAR}" && cargo run --release --bin "day${DAY_STR}")
else
    echo "No solution found for Day ${DAY_STR}, Year ${YEAR}."
    exit 1
fi