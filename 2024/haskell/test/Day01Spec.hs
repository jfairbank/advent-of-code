module Day01Spec (suite) where

import qualified Data.Text.IO as TextIO
import qualified Day01
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
  exampleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day01/example.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day01/puzzle.txt"

  describe "Day 1" $ do
    describe "puzzle1" $ do
      it "solves the example input input" $ do
        Day01.puzzle1 exampleInput `shouldBe` 11

      it "solves the puzzle input input" $ do
        Day01.puzzle1 puzzleInput `shouldBe` 3714264

    describe "puzzle2" $ do
      it "solves the example input input" $ do
        Day01.puzzle2 exampleInput `shouldBe` 31

      it "solves the puzzle input input" $ do
        Day01.puzzle2 puzzleInput `shouldBe` 18805872
