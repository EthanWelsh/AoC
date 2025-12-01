module Day01Spec (spec) where

import           Test.Hspec
import           Text.Megaparsec (parse)
import qualified Day01 as D
import qualified Utils.List as UL

spec :: Spec
spec = describe "Day01.parseInput" $ do
  it "parses the sample input file" $ do
    contents <- readFile "input/day01_sample.txt"
    case parse D.parseInput "day01_sample.txt" contents of
      Left err -> expectationFailure (show err)
      Right input -> do
        UL.count (const True) input `shouldBe` 10
        head input `shouldBe` (D.L 68)
