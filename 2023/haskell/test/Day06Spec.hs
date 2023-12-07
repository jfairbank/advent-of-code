module Day06Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day06
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day06/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day06/puzzle.txt"

  describe "Day 6" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day06.puzzle1 exampleInput `shouldBe` 288

      it "solves the puzzle input" $ do
        Day06.puzzle1 puzzleInput `shouldBe` 227850

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day06.puzzle2 exampleInput `shouldBe` 71503

      it "solves the puzzle input" $ do
        Day06.puzzle2 puzzleInput `shouldBe` 42948149
