{-# LANGUAGE OverloadedStrings #-}
module Year2022.Day13 (solve) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Control.Monad (void)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

type Parser = Parsec Void T.Text

------------ PARSER ------------
packetParser :: Parser Packet
packetParser = choice 
    [ Item <$> L.decimal
    , List <$> between (char '[') (char ']') (packetParser `sepBy` char ',')
    ]

inputParser :: Parser Input
inputParser = packetParser `sepEndBy` some eol

------------ TYPES ------------
data Packet = Item Int | List [Packet] deriving (Eq)

instance Show Packet where
  show (Item x) = show x
  show (List ps) = let packets = map show ps in "[" ++ intercalate "," packets ++ "]"

type Input = [Packet]
type OutputA = String
type OutputB = String

------------ PART A ------------
instance Ord Packet where
  compare (Item l) (Item r) = compare l r
  compare (List []) (List []) = EQ
  compare (List []) (List (_:_)) = LT
  compare (List (_:_)) (List []) = GT
  compare (List l) r@(Item _) = compare (List l) (List [r])
  compare l@(Item _) (List r) = compare (List [l]) (List r)
  compare (List (l:ls)) (List (r:rs)) =
    let headValidity = compare l r
    in if headValidity == EQ then compare (List ls) (List rs) else headValidity

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:y:ys) = (x, y) : pairs ys

isValid :: (Packet, Packet) -> Bool
isValid (l, r) = l < r

partA :: Input -> OutputA
partA input =
  let packetPairs = pairs input
      pairsWithIndex = zip [1..] packetPairs
      onlyValid = filter (\(_, pair) -> isValid pair) pairsWithIndex
  in show $ sum $ map fst onlyValid

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let extras = [List [List [Item 2]], List [List [Item 6]]]
      packets = sort $ extras ++ input
  in show $ product $ map (+1) $ findIndices (`elem` extras) packets

------------ SOLVE ------------
solve :: FilePath -> IO ()
solve filePath = do
  contents <- TIO.readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      putStrLn $ partA input
      putStr "Part 2: "
      putStrLn $ partB input
