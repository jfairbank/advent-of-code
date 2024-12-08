module Day05Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day05
import Paths_advent (getDataFileName)
import Test.Hspec
  ( Spec,
    describe,
    it,
    runIO,
    shouldBe,
  )

suite :: Spec
suite = do
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day05/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day05/puzzle.txt"

  describe "Day 5" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day05.puzzle1 exampleInput `shouldBe` 143

      it "solves the puzzle input" $ do
        Day05.puzzle1 puzzleInput `shouldBe` 6384

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day05.puzzle2 exampleInput `shouldBe` 123

      it "solves the puzzle input" $ do
        Day05.puzzle2 puzzleInput `shouldBe` 5353
