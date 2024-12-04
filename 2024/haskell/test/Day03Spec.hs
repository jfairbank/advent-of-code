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
  example1Input <- runIO $ TextIO.readFile =<< getDataFileName "data/Day03/example1.txt"
  example2Input <- runIO $ TextIO.readFile =<< getDataFileName "data/Day03/example2.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day03/puzzle.txt"

  describe "Day 3" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day03.puzzle1 example1Input `shouldBe` 161

      it "solves the puzzle input" $ do
        Day03.puzzle1 puzzleInput `shouldBe` 166357705

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day03.puzzle2 example2Input `shouldBe` 48

      it "solves the puzzle input" $ do
        Day03.puzzle2 puzzleInput `shouldBe` 88811886
