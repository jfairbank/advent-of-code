module Day02Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day02
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day02/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day02/puzzle.txt"

  describe "Day 2" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day02.puzzle1 exampleInput `shouldBe` 8

      it "solves the puzzle input" $ do
        Day02.puzzle1 puzzleInput `shouldBe` 2239

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day02.puzzle2 exampleInput `shouldBe` 2286

      it "solves the puzzle input" $ do
        Day02.puzzle2 puzzleInput `shouldBe` 83435
