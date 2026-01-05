{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Year2022.Day07 (solve) where

{- ORMOLU_DISABLE -}
import Data.List (filter, map, sum, concatMap, sort, drop)
import Data.Void (Void)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ORMOLU_ENABLE -}

-- $setup
-- >>> import qualified Data.Text.IO as TIO
-- >>> import Text.Megaparsec (parse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> let example = unsafePerformIO $ TIO.readFile "years/Year2022/input/sample/Day07.txt"
-- >>> let Right parsedExample = parse inputParser "" example
-- >>> partA parsedExample
-- 95437
-- >>> partB parsedExample
-- 24933642

type Parser = Parsec Void T.Text

------------ PARSER ------------
cdParser :: Parser Command
cdParser = do
  void $ string "$ cd "
  n <- takeWhileP Nothing (/= '\n')
  void $ eol
  return (CD (T.unpack n))

lsFileResultParser :: Parser Entity
lsFileResultParser = do
  s <- L.decimal
  void $ char ' '
  n <- takeWhileP Nothing (/= '\n')
  return File {name = T.unpack n, size = s}

lsDirResultParser :: Parser Entity
lsDirResultParser = do
  void $ string "dir "
  n <- takeWhileP Nothing (/= '\n')
  return Directory {name = T.unpack n, children = [], parent = Nil}

lsParser :: Parser Command
lsParser = do
  void $ string "$ ls"
  void $ eol
  results <- (lsFileResultParser <|> lsDirResultParser) `sepEndBy` eol
  return (LS results)

inputParser :: Parser Input
inputParser = many (cdParser <|> lsParser)

------------ TYPES ------------

data Command = CD String | LS [Entity] deriving (Eq, Show)

data Entity = Nil | File {name :: String, size :: Int} | Directory {name :: String, children :: [Entity], parent :: Entity} deriving (Eq, Show)

type Input = [Command]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

isDir :: Entity -> Bool
isDir (Directory _ _ _) = True
isDir _ = False

setResults :: Entity -> [Entity] -> Entity
setResults (Directory n _ p) results = (Directory n results p)
setResults _ _ = error "setResults: not a Directory"

setParent :: Entity -> Entity -> Entity
setParent (Directory n r _) p = Directory n r p
setParent (File n s) _ = File n s
setParent Nil _ = error "setParent: Nil entity"

removeChild :: Entity -> String -> Entity
removeChild (Directory n rs p) toRemove =
  let newRs = filter ((/= toRemove) . name) rs
   in (Directory n newRs p)
removeChild _ _ = error "removeChild: not a Directory"

addChild :: Entity -> Entity -> Entity
addChild (Directory n rs p) r = (Directory n (r : rs) p)
addChild _ _ = error "addChild: not a Directory"

repeatUntil :: (a -> a) -> (a -> Bool) -> a -> a
repeatUntil f p a = if (p a) then a else repeatUntil f p (f a)

up :: Entity -> Entity
up focus = addChild (parent focus) focus

hasParent :: Entity -> Bool
hasParent (Directory _ _ Nil) = False
hasParent _ = True

allTheWayUp :: Entity -> Entity
allTheWayUp d = repeatUntil up (not . hasParent) d

setParentAndRemoveSelf :: Entity -> Entity -> Entity
setParentAndRemoveSelf focus p =
  let parentWithoutChild = (removeChild p (name focus))
   in setParent focus parentWithoutChild

commandToEntity :: Entity -> Command -> Entity
commandToEntity focus (LS results) =
  let newFocus = setResults focus results
      newResults = map (\r -> setParentAndRemoveSelf r newFocus) results
   in setResults focus newResults
commandToEntity focus (CD "..") = addChild (parent focus) focus
commandToEntity focus (CD n) = Directory {name = n, children = [], parent = (removeChild focus n)}

commandsToEntity :: [Command] -> Entity
commandsToEntity cs =
  let root = Directory {name = "/", children = [], parent = Nil}
   in helper root cs

helper :: Entity -> [Command] -> Entity
helper focus [] = focus
helper focus (c : cs) = helper (commandToEntity focus c) cs

getEntitySize :: Entity -> Int
getEntitySize (File _ s) = s
getEntitySize (Directory _ cs _) = sum $ map getEntitySize cs
getEntitySize Nil = 0

getAllDescendents :: Entity -> [Entity]
getAllDescendents (Directory n cs p) = (Directory n cs p) : (concatMap getAllDescendents cs)
getAllDescendents file = [file]

partA :: Input -> OutputA
partA input =
  let tree = allTheWayUp $ commandsToEntity (drop 1 input) :: Entity
      allDescendents = getAllDescendents tree
      justDirs = filter isDir allDescendents
      dirSizes = map getEntitySize justDirs
      smallDirs = filter (<= 100000) dirSizes
   in sum smallDirs

------------ PART B ------------
calculateRequiredSpace :: Int -> Int
calculateRequiredSpace usedSpace =
  let totalAvailableSpace = 70000000
      requiredSpace = 30000000
      unusedSpace = totalAvailableSpace - usedSpace
      requiredAdditionalSpace = requiredSpace - unusedSpace
   in requiredAdditionalSpace

partB :: Input -> OutputB
partB input =
  let tree = allTheWayUp $ commandsToEntity (drop 1 input) :: Entity
      usedSpace = getEntitySize tree
      requiredSpace = calculateRequiredSpace usedSpace
      allDescendents = getAllDescendents tree
      justDirs = filter isDir allDescendents
      dirSizes = map getEntitySize justDirs
      bigEnoughDirs = filter (>= requiredSpace) dirSizes
   in head $ sort $ bigEnoughDirs

solve :: FilePath -> IO ()
solve filePath = do
  contents <- TIO.readFile filePath
  case parse inputParser filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      putStr "Part 1: "
      print $ partA input
      putStr "Part 2: "
      print $ partB input
