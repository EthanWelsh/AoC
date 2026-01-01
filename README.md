# ❄️ Advent-Of-Code ❄️

This repository contains my solutions for the Advent of Code puzzles, organized by year.

## Directory Structure

The repository is organized as a mono-repo with the following structure:

- `utils`: Contains shared utility code across all years.
- `years`: Contains the solutions for each year.
  - `YYYY`: Contains the solutions for a specific year.
    - `days`: Contains the source code for each day's solution.
    - `input`: Contains the input files for each day.
      - `sample`: Contains sample input files.
      - `real`: Contains the real input files.

## Run Instructions

To run a solution for a specific day, use the following command:

```bash
stack exec aoc -- <year> <day> <sample|real>
```

For example, to run the solution for Day 1 of 2025 with the real input:

```bash
stack exec aoc -- 2025 1 real
```

To run with the sample input:

```bash
stack exec aoc -- 2025 1 sample
```