module Day08Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day08
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day08/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day08/puzzle.txt"

  describe "Day 8" $ do
    describe "puzzle1" $ do
      it "solves the example input" $ do
        Day08.puzzle1 exampleInput `shouldBe` 6

      it "solves the puzzle input" $ do
        Day08.puzzle1 puzzleInput `shouldBe` 12643

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day08.puzzle2 exampleInput `shouldBe` 6

      it "solves the puzzle input" $ do
        Day08.puzzle2 puzzleInput `shouldBe` 13133452426987
