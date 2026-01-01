module Year2022.Day11 (solve) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void, replicateM_)
import           Control.Monad.State        as State
import           Data.List
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vec
import           Parsers                    (Parser)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Item = Int
type Index = Int

data Test = Test Int Index Index
data Monkey = Monkey { getIndex :: Int
                     , getItems :: [Item]
                     , getOperation :: (Item -> Int)
                     , getDecision :: Test
                     , getObservationCount :: Int }

instance Show Monkey where
  show monkey = "{MONKEY " ++ show (getIndex monkey) ++ ", " ++ show (getItems monkey) ++ "}"

type Input = [Monkey]

indexParser :: Parser Int
indexParser = do
    void $ string "Monkey "
    index <- L.decimal
    void $ char ':'
    void eol
    return index

startingItemsParser :: Parser [Int]
startingItemsParser = do
    void $ string "  Starting items: "
    items <- L.decimal `sepBy` (string ", ")
    void eol
    return items

operationParser :: Parser (Item -> Int)
operationParser = do
    void $ string "  Operation: new = old "
    opr <- (char '+') <|> (char '*')
    void $ char ' '
    numberOrOld <- eitherP (L.decimal) (string "old")
    void eol
    return (case numberOrOld of
        Left num    -> if opr == '+' then (\old -> old + num) else (\old -> old * num)
        Right "old" -> if opr == '+' then (\old -> old + old) else (\old -> old * old))

divisibleBy :: Int -> Int -> Bool
divisibleBy n x = x `rem` n == 0

testParser :: Parser Int
testParser = do
    void $ string "  Test: divisible by "
    n <- L.decimal
    void eol
    return n

trueParser :: Parser Int
trueParser = do
    void $ string "    If true: throw to monkey "
    index <- L.decimal
    void eol
    return index

falseParser :: Parser Int
falseParser = do
    void $ string "    If false: throw to monkey "
    index <- L.decimal
    void eol <|> eof
    return index

decisionParser :: Parser Test
decisionParser = do
    test <- testParser
    trueIndex <- trueParser
    falseIndex <- falseParser
    return $ (Test test trueIndex falseIndex)

monkeyParser :: Parser Monkey
monkeyParser = do
    index <- indexParser
    items <- startingItemsParser
    operation <- operationParser
    decision <- decisionParser
    return (Monkey index items operation decision 0)

inputParser :: Parser Input
inputParser = monkeyParser `sepEndBy` eol

makeDecision :: Test -> Int -> Int
makeDecision (Test d t f) n = if (n `rem` d) == 0 then t else f

reduceWorryLevel :: Int -> Int
reduceWorryLevel x = x `div` 3

removeItemFromMonkey :: Int -> Monkey -> Monkey
removeItemFromMonkey item monkey = monkey { getItems = delete item (getItems monkey) }

addItemToMonkey :: Int -> Monkey -> Monkey
addItemToMonkey item monkey = monkey { getItems = (getItems monkey) ++ [item] }

getMonkey :: Int -> State (Vector Monkey) Monkey
getMonkey index = do
    monkeys <- get
    let monkey = monkeys Vec.! index
    return monkey

putMonkey :: Monkey -> State (Vector Monkey) ()
putMonkey monkey = do
    let index = getIndex monkey
    monkeys <- get
    put $ monkeys Vec.// [(index, monkey)]
    return ()

incrementObservations :: Int -> Int -> State (Vector Monkey) ()
incrementObservations index count = do
    monkey <- getMonkey index
    let updatedMonkey = monkey { getObservationCount = (getObservationCount monkey) + count }
    putMonkey updatedMonkey
    return ()

takeTurn :: (Int -> Int) -> Int -> State (Vector Monkey) ()
takeTurn f fromIndex = do
    fromMonkey <- getMonkey fromIndex
    let items = getItems fromMonkey
    case items of
        [] -> return ()
        (x:_) -> do
            let worryLevel = (getOperation fromMonkey) x

            let reducedWorry = f worryLevel

            let test = getDecision fromMonkey
            let toIndex = makeDecision test reducedWorry
            toMonkey <- getMonkey toIndex

            let updatedFrom = removeItemFromMonkey x fromMonkey
            let updatedTo = addItemToMonkey reducedWorry toMonkey

            putMonkey updatedFrom
            putMonkey updatedTo

            incrementObservations fromIndex 1

            takeTurn f fromIndex
            return ()

playRounds :: (Int -> Int) -> State (Vector Monkey) ()
playRounds f = do
    monkeys <- get
    mapM_ (takeTurn f) [0..((length monkeys) - 1)]
    return ()

getCounts :: State (Vector Monkey) [Int]
getCounts = do
    monkeys <- get
    return $ Vec.toList $ Vec.map getObservationCount monkeys

getItemsForEachMonkey :: State (Vector Monkey) [[Int]]
getItemsForEachMonkey = do
    monkeys <- get
    return $ Vec.toList $ Vec.map getItems monkeys

playA :: Int -> State (Vector Monkey) Int
playA rounds = do
    replicateM_ rounds (playRounds reduceWorryLevel)
    counts <- getCounts
    let sorted = (reverse . sort) counts :: [Int]
    let topTwo = Data.List.take 2 sorted :: [Int]
    let score = Data.List.product topTwo
    return score

partA :: Input -> Int
partA input = evalState (playA 20) (Vec.fromList input)

getTestsForEachMonkey :: State (Vector Monkey) [Test]
getTestsForEachMonkey = do
    monkeys <- get
    return $ Vec.toList $ Vec.map getDecision monkeys

playB :: Int -> State (Vector Monkey) Int
playB rounds = do
    tests <- getTestsForEachMonkey
    let divs = map (\(Test n _ _) -> n) tests
    let prodDivs = Data.List.product divs
    let reduceB = (`mod` prodDivs)
    replicateM_ rounds (playRounds reduceB)
    counts <- getCounts
    let sorted = (reverse . sort) counts :: [Int]
    let topTwo = Data.List.take 2 sorted :: [Int]
    let score = Data.List.product topTwo
    return score

partB :: Input -> Int
partB input = evalState (playB 10000) (Vec.fromList input)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: "
      print $ partB input
