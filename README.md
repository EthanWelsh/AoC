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

To run a solution for a specific day, use the `run_day.zsh` script:

```bash
./run_day.zsh [--interactive|-i] <year> <day> <sample|real>
```

**Interactive Mode:**

To run a solution in interactive mode (e.g., with `ghcid` for Haskell or `cargo watch` for Rust), use the `--interactive` or `-i` flag:

```bash
./run_day.zsh --interactive 2025 1 real
```

This will start a live-reloading development environment, automatically re-running your solution whenever you save changes to the code.

**Normal Run:**

For example, to run the solution for Day 1 of 2025 with the real input:

```bash
./run_day.zsh 2025 1 real
```

To run with the sample input:

```bash
./run_day.zsh 2025 1 sample
```

The script will automatically detect if the solution for the given day is written in Haskell or Rust and execute it accordingly. If the input file does not exist, it will attempt to download it using `curl`.