module Day03Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day03
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day03/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day03/puzzle.txt"

  describe "Day 3" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day03.puzzle1 exampleInput `shouldBe` 4361

      it "solves the puzzle input" $ do
        Day03.puzzle1 puzzleInput `shouldBe` 533775

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day03.puzzle2 exampleInput `shouldBe` 467835

      it "solves the puzzle input" $ do
        Day03.puzzle2 puzzleInput `shouldBe` 78236071
