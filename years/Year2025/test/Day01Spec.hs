module Day01Spec (spec) where

import           Test.Hspec
import           Text.Megaparsec (parse)
import qualified Year2025.Day01 as D

spec :: Spec
spec = describe "Day01.parseInput" $ do
  it "parses the sample input file" $ do
    contents <- readFile "years/Year2025/input/sample/Day01.txt"
    case parse D.parseInput "day01_sample.txt" contents of
      Left err -> expectationFailure (show err)
      Right input -> do
        length input `shouldBe` 10
        head input `shouldBe` (D.L 68)
