module Year2022.Day16 (solve) where

import           Algorithm.Search
import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.Function              (on)
import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Data.Set                   as Set
import           Parsers                    (Parser)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Input = Graph
type Graph = Map.Map String Valve
type Costs = Map.Map String (Map.Map String Int)

data Valve = Valve { valvePressure  :: Int
                   , valveNeighbors :: Set.Set String } deriving (Eq, Show, Ord)

data State = State { location          :: String
                   , minute            :: Int
                   , pressurePerMinute :: Int
                   , totalPressure     :: Int
                   , openValves        :: Set.Set String } deriving (Eq, Show, Ord)

valveParser :: Parser String
valveParser = count 2 letterChar

singleValve :: Parser [String]
singleValve = do
    void $ string "; tunnel leads to valve "
    valve <- valveParser
    return [valve]

multipleValve :: Parser [String]
multipleValve = do
    void $ string "; tunnels lead to valves "
    valveParser `sepBy` string ", "

lineParser :: Parser (String, Valve)
lineParser = do
    void $ string "Valve "
    origin <- valveParser
    void $ string " has flow rate="
    flow <- L.decimal
    valves <- singleValve <|> multipleValve
    return (origin, Valve flow (Set.fromList valves))

inputParser :: Parser Graph
inputParser = do
    valves <- lineParser `sepEndBy` eol
    return $ Map.fromList valves

startState :: State
startState = State { location = "AA", minute = 0, pressurePerMinute = 0, totalPressure = 0, openValves = Set.empty }

helpfulValves :: Graph -> [String]
helpfulValves graph = Map.keys $ Map.filter ((> 0) . valvePressure) graph

pathCost :: Graph -> String -> String -> Int
pathCost graph origin destination = fst . fromJust $ dijkstra neighbors cost isGoal origin
    where
        neighbors v = Set.toList $ valveNeighbors (graph Map.! v)
        cost _ _ = 1
        isGoal = (== destination)

mapify :: (Ord a, Ord b) => [(a, b, c)] -> Map.Map a (Map.Map b c)
mapify = foldl' (\m (o, d, c) -> Map.insertWith Map.union o (Map.singleton d c) m) Map.empty

getCosts :: Graph -> Costs
getCosts graph = let
    helpful = helpfulValves graph
    allPaths = [(o, d) | o <- "AA":helpful, d <- helpful, o /= d]
    pathsWithCost = map (\(o, d) -> (o, d, pathCost graph o d)) allPaths :: [(String, String, Int)]
    in mapify pathsWithCost

costFromTo :: Costs -> String -> String -> Int
costFromTo costs origin destination = (costs Map.! origin) Map.! destination

visitDestination :: Graph -> Costs -> State -> String -> Maybe State
visitDestination graph costs state destination = let
    origin = location state
    costToTravel = costFromTo costs origin destination
    costToOpenValve = 1
    timeElapsed = costToTravel + costToOpenValve
    newMinute = minute state + timeElapsed
    newTotalPressure = totalPressure state + timeElapsed * pressurePerMinute state
    newPressurePerMinute = pressurePerMinute state + valvePressure (graph Map.! destination)
    newOpenValves = Set.insert destination (openValves state)
    timeUp = newMinute > 30
    in if timeUp then Nothing else Just $ state {
        location = destination,
        minute = newMinute,
        totalPressure = newTotalPressure,
        pressurePerMinute = newPressurePerMinute,
        openValves = newOpenValves }

totalPressureAtEnd :: State -> Int
totalPressureAtEnd state = let
    timeRemaining = 30 - minute state
    in totalPressure state + (timeRemaining * pressurePerMinute state)

visitAll :: Graph -> Costs -> State -> State
visitAll graph costs state = let
    origin = location state                                                        :: String
    unopened = filter (`Set.notMember` openValves state) . Map.keys $ costs Map.! origin :: [String]
    children = mapMaybe (visitDestination graph costs state) unopened              :: [State]
    bestForEachChild = map (visitAll graph costs) children                         :: [State]
    bestOverall = maximumBy (compare `on` totalPressure) bestForEachChild          :: State
    timeRemaining = 30 - minute state
    pressureIfWaitTillEnd = totalPressure state + timeRemaining * pressurePerMinute state
    in if null children
        then state { minute = 30, totalPressure = pressureIfWaitTillEnd }
        else bestOverall

partA :: Input -> String
partA graph = do
    let costs = getCosts graph
    show $ totalPressureAtEnd $ visitAll graph costs startState

partB :: Input -> String
partB graph = show "hello"

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      putStrLn $ partA input
      putStr "Part 2: "
      putStrLn $ partB input