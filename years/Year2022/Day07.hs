module Year2022.Day07 (solve) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Parsers                    (Parser)

data Command = CD String | LS [Entity] deriving (Eq, Show)
data Entity = Nil | File { name :: String, size :: Int } | Directory { name :: String, children :: [Entity], parent :: Entity } deriving (Eq, Show)
type Input = [Command]

cdParser :: Parser Command
cdParser = do
    void $ string "$ cd "
    name <- some (noneOf "\n")
    void eol
    return (CD name)

lsFileResultParser :: Parser Entity
lsFileResultParser = do
    size <- L.decimal
    void $ char ' '
    name <- some (noneOf "\n")
    return File {name = name, size = size}

lsDirResultParser :: Parser Entity
lsDirResultParser = do
    void $ string "dir "
    name <- some (noneOf "\n")
    return Directory { name = name, children = [], parent = Nil}

lsParser :: Parser Command
lsParser = do
    void $ string "$ ls"
    void eol
    results <- (lsFileResultParser <|> lsDirResultParser) `sepEndBy` eol
    return (LS results)

inputParser :: Parser Input
inputParser = some (cdParser <|> lsParser)

isDir :: Entity -> Bool
isDir (Directory _ _ _) = True
isDir _ = False

setResults :: Entity -> [Entity] -> Entity
setResults (Directory n _ p) results = (Directory n results p)
setResults f _ = f

setParent :: Entity -> Entity -> Entity
setParent (Directory n r _) p = Directory n r p
setParent (File n s) _ = File n s

removeChild :: Entity -> String -> Entity
removeChild (Directory n rs p) toRemove = let
    newRs = filter ((/= toRemove) . name) rs
    in (Directory n newRs p)
removeChild f _ = f

addChild :: Entity -> Entity -> Entity
addChild (Directory n rs p) r = (Directory n (r:rs) p)
addChild f _ = f

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
setParentAndRemoveSelf focus parent' = let
    parentWithoutChild = (removeChild parent' (name focus))
    in setParent focus parentWithoutChild

commandToEntity :: Entity -> Command -> Entity
commandToEntity focus (LS results) = let
    newFocus = setResults focus results
    newResults = map (\r -> setParentAndRemoveSelf r newFocus) results
    in setResults focus newResults
commandToEntity focus (CD "..") = addChild (parent focus) focus
commandToEntity focus (CD n) = Directory { name = n, children = [], parent = (removeChild focus n) }

commandsToEntity :: [Command] -> Entity
commandsToEntity cs = let
    root = Directory { name = "/", children = [], parent = Nil}
    in helper root cs

helper :: Entity -> [Command] -> Entity
helper focus [] = focus
helper focus (c:cs) = helper (commandToEntity focus c) cs

getEntitySize :: Entity -> Int
getEntitySize (File _ s) = s
getEntitySize (Directory _ cs _) = sum $ map getEntitySize cs

getAllDescendents :: Entity -> [Entity]
getAllDescendents (Directory n cs p) = (Directory n cs p) : (concatMap getAllDescendents cs)
getAllDescendents file = [file]

partA :: Input -> Int
partA input = let
    tree = allTheWayUp $ commandsToEntity (drop 1 input) :: Entity
    allDescendents = getAllDescendents tree
    justDirs = filter isDir allDescendents
    dirSizes = map getEntitySize justDirs
    smallDirs = filter (<=100000) dirSizes
    in sum smallDirs

calculateRequiredSpace :: Int -> Int
calculateRequiredSpace usedSpace = let
    totalAvailableSpace = 70000000
    requiredSpace = 30000000
    unusedSpace = totalAvailableSpace - usedSpace
    requiredAdditionalSpace = requiredSpace - unusedSpace
    in requiredAdditionalSpace

partB :: Input -> Int
partB input = let
    tree = allTheWayUp $ commandsToEntity (drop 1 input) :: Entity
    usedSpace = getEntitySize tree
    requiredSpace = calculateRequiredSpace usedSpace
    allDescendents = getAllDescendents tree
    justDirs = filter isDir allDescendents
    dirSizes = map getEntitySize justDirs
    bigEnoughDirs = filter (>=requiredSpace) dirSizes
    in head $ sort $ bigEnoughDirs

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