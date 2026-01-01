module Year2022.Day13 (solve) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.List
import           Data.Functor               (($>))
import           Parsers                    (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Packet = Item Int | List [Packet] deriving (Eq)

instance Show Packet where
  show (Item x) = show x
  show (List ps) = let
    packets = map show ps
    in "[" ++ (intercalate "," packets) ++ "]"

instance Ord Packet where
    compare (Item l) (Item r) = compare l r
    compare (List []) (List []) = EQ
    compare (List []) (List (r:rs)) = LT
    compare (List (l:ls)) (List []) = GT
    compare (List l) r@(Item _) = compare (List l) (List [r])
    compare l@(Item _) (List r) = compare (List [l]) (List r)
    compare (List (l:ls)) (List (r:rs)) = let
        headValidity = compare l r
        in if headValidity == EQ then compare (List ls) (List rs) else headValidity

type Input = [Packet]

packetParser :: Parser Packet
packetParser = choice [Item <$> L.decimal, List <$> between (single '[') (single ']') (packetParser `sepBy` single ',')]

inputParser :: Parser Input
inputParser = packetParser `sepEndBy` some eol

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:y:ys) = (x, y) : (pairs ys)

isValid :: (Packet, Packet) -> Bool
isValid (l, r) = l < r

partA :: Input -> String
partA input = let
    packetPairs = pairs input
    pairsWithIndex = zip [1..] packetPairs
    onlyValid = filter (\(i, pair) -> isValid pair) pairsWithIndex
    in show $ sum $ map fst onlyValid

partB :: Input -> String
partB input = let
    extras = [List[List[Item 2]], List[List[Item 6]]]
    packets = sort $ (extras ++) $ input
    in show $ product $ map (+1) $ findIndices (`elem` extras) packets

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