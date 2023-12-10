module Day09Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day09
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day09/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day09/puzzle.txt"

  describe "Day 9" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day09.puzzle1 exampleInput `shouldBe` 114

      it "solves the puzzle input" $ do
        Day09.puzzle1 puzzleInput `shouldBe` 1939607039

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day09.puzzle2 exampleInput `shouldBe` 2

      it "solves the puzzle input" $ do
        Day09.puzzle2 puzzleInput `shouldBe` 1041
