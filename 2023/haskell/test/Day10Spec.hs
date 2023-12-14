module Day10Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day10
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
  exampleInput1 <- runIO $ TextIO.readFile =<< getDataFileName "data/Day10/example1.txt"
  exampleInput2 <- runIO $ TextIO.readFile =<< getDataFileName "data/Day10/example2.txt"
  exampleInput3 <- runIO $ TextIO.readFile =<< getDataFileName "data/Day10/example3.txt"
  exampleInput4 <- runIO $ TextIO.readFile =<< getDataFileName "data/Day10/example4.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day10/puzzle.txt"

  describe "Day 10" $ do
    describe "puzzle1" $ do
      it "solves the example1 input" $ do
        Day10.puzzle1 exampleInput1 `shouldBe` 8

      it "solves the puzzle input" $ do
        Day10.puzzle1 puzzleInput `shouldBe` 6860

    describe "puzzle2" $ do
      it "solves the example2 input" $ do
        Day10.puzzle2 exampleInput2 `shouldBe` 4

      it "solves the example3 input" $ do
        Day10.puzzle2 exampleInput3 `shouldBe` 8

      it "solves the example4 input" $ do
        Day10.puzzle2 exampleInput4 `shouldBe` 10

      it "solves the puzzle input" $ do
        Day10.puzzle2 puzzleInput `shouldBe` 343
