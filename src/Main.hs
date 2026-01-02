module Main (main) where


import qualified Year2025.Day01
import qualified Year2025.Day02
import qualified Year2025.Day03
import qualified Year2025.Day04
import qualified Year2025.Day05
import qualified Year2025.Day06
import qualified Year2025.Day07
import qualified Year2025.Day08
import qualified Year2025.Day09
import qualified Year2025.Day10
import qualified Year2025.Day11
import qualified Year2025.Day12
import qualified Year2023.Day01
import qualified Year2023.Day02
import qualified Year2023.Day03
import qualified Year2023.Day04
import qualified Year2023.Day05
import qualified Year2023.Day06
import qualified Year2023.Day07
import qualified Year2023.Day08
import qualified Year2023.Day09
import qualified Year2023.Day10
import qualified Year2023.Day11
import qualified Year2023.Day12
import qualified Year2023.Day13
import qualified Year2023.Day14
import qualified Year2023.Day15
import qualified Year2023.Day16
import qualified Year2023.Day17
import qualified Year2023.Day18
import qualified Year2023.Day19
import qualified Year2023.Day20
import qualified Year2023.Day21
import qualified Year2023.Day23
import qualified Year2023.Day24
import qualified Year2023.Day25
import qualified Year2022.Day01
import qualified Year2022.Day02
import qualified Year2022.Day03
import qualified Year2022.Day04
import qualified Year2022.Day05
import qualified Year2022.Day06
import qualified Year2022.Day07
import qualified Year2022.Day08
import qualified Year2022.Day09
import qualified Year2022.Day10
import qualified Year2022.Day11
import qualified Year2022.Day12
import qualified Year2022.Day13
import qualified Year2022.Day14
import qualified Year2022.Day15
import qualified Year2022.Day16
import qualified Year2022.Day17
import qualified Year2022.Day18
import qualified Year2022.Day19
import qualified Year2022.Day20
import qualified Year2022.Day21
import qualified Year2022.Day22
import qualified Year2022.Day23
import qualified Year2022.Day24
import qualified Year2022.Day25
import qualified Year2019.Day01
import qualified Year2019.Day02
import qualified Year2019.Day03
import qualified Year2019.Day04
import qualified Year2019.Day05
import qualified Year2019.Day06
import qualified Year2019.Day07
import qualified Year2019.Day08
import qualified Year2019.Day09
import qualified Year2019.Day10
import qualified Year2019.Day11
import qualified Year2019.Day12
import qualified Year2019.Day13
import qualified Year2019.Day14
import qualified Year2019.Day15
import qualified Year2019.Day16
import qualified Year2019.Day17
import qualified Year2019.Day18
import qualified Year2019.Day19
import qualified Year2019.Day20
import qualified Year2019.Day21
import qualified Year2019.Day22
import qualified Year2019.Day23
import qualified Year2019.Day24
import qualified Year2019.Day25
import qualified Year2021.Day01
import qualified Year2021.Day02
import qualified Year2021.Day03
import qualified Year2021.Day04


import qualified Year2021.Day06
import qualified Year2021.Day07
import qualified Year2021.Day08
import qualified Year2021.Day09
import qualified Year2021.Day10
import qualified Year2021.Day11
import qualified Year2021.Day12
import qualified Year2021.Day13
import qualified Year2021.Day14
import qualified Year2021.Day15
import qualified Year2021.Day16
import qualified Year2021.Day17
import qualified Year2021.Day18
import qualified Year2021.Day19
import qualified Year2021.Day20
import qualified Year2021.Day21
import qualified Year2021.Day22
import qualified Year2021.Day23
import qualified Year2021.Day24
import qualified Year2021.Day25

import           System.Environment (getArgs)
import           Text.Printf (printf)
import           Data.Map (Map)
import qualified Data.Map as Map

solvers2025 :: [FilePath -> IO ()]
solvers2025 =
    [Year2025.Day01.solve
    , Year2025.Day02.solve
    , Year2025.Day03.solve
    , Year2025.Day04.solve
    , Year2025.Day05.solve
    , Year2025.Day06.solve
    , Year2025.Day07.solve
    , Year2025.Day08.solve
    , Year2025.Day09.solve
    , Year2025.Day10.solve
    , Year2025.Day11.solve
    , Year2025.Day12.solve
    ]

solvers2023 :: [FilePath -> IO ()]
solvers2023 =
    [ Year2023.Day01.solve
    , Year2023.Day02.solve
    , Year2023.Day03.solve
    , Year2023.Day04.solve
    , Year2023.Day05.solve
    , Year2023.Day06.solve
    , Year2023.Day07.solve
    , Year2023.Day08.solve
    , Year2023.Day09.solve
    , Year2023.Day10.solve
    , Year2023.Day11.solve
    , Year2023.Day12.solve
    , Year2023.Day13.solve
    , Year2023.Day14.solve
    , Year2023.Day15.solve
    , Year2023.Day16.solve
    , Year2023.Day17.solve
    , Year2023.Day18.solve
    , Year2023.Day19.solve
    , Year2023.Day20.solve
    , Year2023.Day21.solve
    , Year2023.Day23.solve
    , Year2023.Day24.solve
    , Year2023.Day25.solve
    ]




solvers2022 :: [FilePath -> IO ()]
solvers2022 =
    [ Year2022.Day01.solve
    , Year2022.Day02.solve
    , Year2022.Day03.solve
    , Year2022.Day04.solve
    , Year2022.Day05.solve
    , Year2022.Day06.solve
    , Year2022.Day07.solve
    , Year2022.Day08.solve
    , Year2022.Day09.solve
    , Year2022.Day10.solve
    , Year2022.Day11.solve
    , Year2022.Day12.solve
    , Year2022.Day13.solve
    , Year2022.Day14.solve
    , Year2022.Day15.solve
    , Year2022.Day16.solve
    , Year2022.Day17.solve
    , Year2022.Day18.solve
    , Year2022.Day19.solve
    , Year2022.Day20.solve
    , Year2022.Day21.solve
    , Year2022.Day22.solve
    , Year2022.Day23.solve
    , Year2022.Day24.solve
    , Year2022.Day25.solve
    ]

solvers2019 :: [FilePath -> IO ()]
solvers2019 =
    [ Year2019.Day01.solve
    , Year2019.Day02.solve
    , Year2019.Day03.solve
    , Year2019.Day04.solve
    , Year2019.Day05.solve
    , Year2019.Day06.solve
    , Year2019.Day07.solve
    , Year2019.Day08.solve
    , Year2019.Day09.solve
    , Year2019.Day10.solve
    , Year2019.Day11.solve
    , Year2019.Day12.solve
    , Year2019.Day13.solve
    , Year2019.Day14.solve
    , Year2019.Day15.solve
    , Year2019.Day16.solve
    , Year2019.Day17.solve
    , Year2019.Day18.solve
    , Year2019.Day19.solve
    , Year2019.Day20.solve
    , Year2019.Day21.solve
    , Year2019.Day22.solve
    , Year2019.Day23.solve
    , Year2019.Day24.solve
    , Year2019.Day25.solve
    ]

solvers2021 :: [FilePath -> IO ()]
solvers2021 =
    [ Year2021.Day01.solve
    , Year2021.Day02.solve
    , Year2021.Day03.solve
    , Year2021.Day04.solve
    
    , Year2021.Day06.solve
    , Year2021.Day07.solve
    , Year2021.Day08.solve
    , Year2021.Day09.solve
    , Year2021.Day10.solve
    , Year2021.Day11.solve
    , Year2021.Day12.solve
    , Year2021.Day13.solve
    , Year2021.Day14.solve
    , Year2021.Day15.solve
    , Year2021.Day16.solve
    , Year2021.Day17.solve
    , Year2021.Day18.solve
    , Year2021.Day19.solve
    , Year2021.Day20.solve
    , Year2021.Day21.solve
    , Year2021.Day22.solve
    , Year2021.Day23.solve
    , Year2021.Day24.solve
    , Year2021.Day25.solve
    ]

solvers :: Map String [FilePath -> IO ()]
solvers = Map.fromList
    [ ("2025", solvers2025)
    , ("2023", solvers2023)
    , ("2022", solvers2022)
    , ("2019", solvers2019)
    , ("2021", solvers2021)
    ]

main :: IO ()
main = do
    [year, day, sample] <- getArgs
    let yearSolvers = solvers Map.! year
        solver = if year == "2025"
                    then yearSolvers !! read day
                    else yearSolvers !! (read day - 1)
        dayStr :: String
        dayStr = printf "%02d" (read day :: Int)
        sampleStr = if sample == "sample" then "sample" else "real"
        filePath :: String
        filePath = printf "years/Year%s/input/%s/Day%s.txt" year sampleStr dayStr
    putStrLn ""
    solver filePath