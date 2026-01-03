module Main where

import Test.DocTest
import System.Directory
import System.FilePath
import Control.Monad

main :: IO ()
main = do
    files <- getDirectoryContentsRecursive "years"
    let hsFiles = filter (\f -> takeExtension f == ".hs") files
    doctest $ "-isrc" : "-iutils" : "-iyears" : hsFiles

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    names <- listDirectory dir
    paths <- forM names $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getDirectoryContentsRecursive path
            else return [path]
    return (concat paths)