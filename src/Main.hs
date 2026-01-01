module Main (main) where

import qualified Year2025.Day00
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
import qualified Year2023.Day10
import qualified Year2023.Day12
import qualified Year2023.Day15
import qualified Year2023.Day17
import qualified Year2023.Day21
import qualified Year2023.Day23
import qualified Year2023.Day24
import qualified Year2023.Day25

import           System.Environment (getArgs)
import           Text.Printf (printf)
import           Data.Map (Map)
import qualified Data.Map as Map

solvers2025 :: [FilePath -> IO ()]
solvers2025 =
    [ Year2025.Day00.solve
    , Year2025.Day01.solve
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
    , (\_ -> putStrLn "Day 9 not implemented")
    , Year2023.Day10.solve
    , (\_ -> putStrLn "Day 11 not implemented")
    , Year2023.Day12.solve
    , (\_ -> putStrLn "Day 13 not implemented")
    , (\_ -> putStrLn "Day 14 not implemented")
    , Year2023.Day15.solve
    , (\_ -> putStrLn "Day 16 not implemented")
    , Year2023.Day17.solve
    , (\_ -> putStrLn "Day 18 not implemented")
    , (\_ -> putStrLn "Day 19 not implemented")
    , (\_ -> putStrLn "Day 20 not implemented")
    , Year2023.Day21.solve
    , (\_ -> putStrLn "Day 22 not implemented")
    , Year2023.Day23.solve
    , Year2023.Day24.solve
    , Year2023.Day25.solve
    ]

solvers :: Map String [FilePath -> IO ()]
solvers = Map.fromList
    [ ("2025", solvers2025)
    , ("2023", solvers2023)
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