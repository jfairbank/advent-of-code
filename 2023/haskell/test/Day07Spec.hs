module Day07Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day07
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day07/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day07/puzzle.txt"

  describe "Day 7" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day07.puzzle1 exampleInput `shouldBe` 6440

      it "solves the puzzle input" $ do
        Day07.puzzle1 puzzleInput `shouldBe` 250347426

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day07.puzzle2 exampleInput `shouldBe` 5905

      it "solves the puzzle input" $ do
        Day07.puzzle2 puzzleInput `shouldBe` 251224870
