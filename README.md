# ❄️ Advent-Of-Code ❄️

This repository contains my solutions for the Advent of Code puzzles, organized by year.

## Reading Guide for Solution Status

This table provides a concise overview of the completion status for each Advent of Code puzzle.

-   **H**: Haskell solution exists, and both Part 1 and Part 2 are complete.
-   **H1**: Haskell solution exists, Part 1 is complete, but Part 2 is incomplete.
-   **R**: Rust solution exists, and both Part 1 and Part 2 are complete.
-   **R1**: Rust solution exists, Part 1 is complete, but Part 2 is incomplete.
-   **S**: A solution exists, but it was skipped due to complex logic issues that could not be resolved in this environment.
-   **-**: The solution for this day does not exist in the repository, or it is a placeholder (not implemented).

## Solution Status

| Year | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 |
|------|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 2015 | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
| 2016 | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
| 2017 | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
| 2018 | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
| 2019 | [H](./years/Year2019/Day01.hs) | [H](./years/Year2019/Day02.hs) | [H](./years/Year2019/Day03.hs) | [H](./years/Year2019/Day04.hs) | [H](./years/Year2019/Day05.hs) | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
| 2020 | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
| 2021 | [H](./years/Year2021/Day01.hs) | - | [H](./years/Year2021/Day03.hs) | [H](./years/Year2021/Day04.hs) | [H](./years/Year2021/Day05.hs) | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
| 2022 | [H](./years/Year2022/Day01.hs) | [H](./years/Year2022/Day02.hs) | [H](./years/Year2022/Day03.hs) | [H](./years/Year2022/Day04.hs) | [H](./years/Year2022/Day05.hs) | [H](./years/Year2022/Day06.hs) | [H](./years/Year2022/Day07.hs) | [H](./years/Year2022/Day08.hs) | [H](./years/Year2022/Day09.hs) | [H](./years/Year2022/Day10.hs) | - | [H](./years/Year2022/Day12.hs) | [H](./years/Year2022/Day13.hs) | [H](./years/Year2022/Day14.hs) | [H](./years/Year2022/Day15.hs) | - | - | [H](./years/Year2022/Day18.hs) | - | - | - | - | - | - | - |
| 2023 | [H](./years/Year2023/Day01.hs) | [H](./years/Year2023/Day02.hs) | [H](./years/Year2023/Day03.hs) | [H](./years/Year2023/Day04.hs) | [H](./years/Year2023/Day05.hs) | [H](./years/Year2023/Day06.hs) | [H](./years/Year2023/Day07.hs) | [H](./years/Year2023/Day08.hs) | - | [S](./years/Year2023/Day10.hs) | - | [S](./years/Year2023/Day12.hs) | - | - | [H](./years/Year2023/Day15.hs) | - | [S](./years/Year2023/Day17.hs) | - | - | - | [H1](./years/Year2023/Day21.hs) | - | [S](./years/Year2023/Day23.hs) | - | [S](./years/Year2023/Day25.hs) |
| 2024 | [R](./years/Year2024/Day01.rs) | [R](./years/Year2024/Day02.rs) | [R](./years/Year2024/Day03.rs) | [R](./years/Year2024/Day04.rs) | [R](./years/Year2024/Day05.rs) | [R](./years/Year2024/Day06.rs) | [R](./years/Year2024/Day07.rs) | [R](./years/Year2024/Day08.rs) | [R](./years/Year2024/Day09.rs) | [R](./years/Year2024/Day10.rs) | [R](./years/Year2024/Day11.rs) | [R](./years/Year2024/Day12.rs) | - | - | [R](./years/Year2024/Day15.rs) | [R](./years/Year2024/Day16.rs) | - | [R](./years/Year2024/Day18.rs) | [R](./years/Year2024/Day19.rs) | [R](./years/Year2024/Day20.rs) | - | - | - | - | - |
| 2025 | [H](./years/Year2025/Day01.hs) | [H](./years/Year2025/Day02.hs) | [H](./years/Year2025/Day03.hs) | [H](./years/Year2025/Day04.hs) | [H](./years/Year2025/Day05.hs) | [H](./years/Year2025/Day06.hs) | [H](./years/Year2025/Day07.hs) | [H](./years/Year2025/Day08.hs) | [H](./years/Year2025/Day09.hs) | [H](./years/Year2025/Day10.hs) | [H](./years/Year2025/Day11.hs) | [H](./years/Year2025/Day12.hs) |   |   |   |   |   |   |   |   |   |   |   |   |   |

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

## Creating a New Day

To create a new day, use the `create_new_day.zsh` script:

```bash
./create_new_day.zsh <year> <day> <language>
```

For example, to create a new Haskell solution for Day 25 of 2024:

```bash
./create_new_day.zsh 2024 25 haskell
```

To create a new Rust solution for Day 25 of 2024:

```bash
./create_new_day.zsh 2024 25 rust
```

The script will create the necessary files and update the build configurations.

## Running Tests

This project uses `doctest` to run tests embedded in the source code. To run the tests, you will need to have `stack` installed.

You can run the tests for a specific day by running the following command:

```bash
stack exec -- doctest -isrc -iutils -iyears years/Year2025/DayXX.hs
```

Replace `XX` with the day number you want to test.

You can also run all the tests at once by running:

```bash
stack test
```