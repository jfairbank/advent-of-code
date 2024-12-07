module Day04Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day04
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day04/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day04/puzzle.txt"

  describe "Day 4" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day04.puzzle1 exampleInput `shouldBe` 18

      it "solves the puzzle input" $ do
        Day04.puzzle1 puzzleInput `shouldBe` 2639

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day04.puzzle2 exampleInput `shouldBe` 9

      it "solves the puzzle input" $ do
        Day04.puzzle2 puzzleInput `shouldBe` 2005
