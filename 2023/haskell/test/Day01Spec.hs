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
  exampleInput1 <- runIO $ TextIO.readFile =<< getDataFileName "data/Day01/example1.txt"
  exampleInput2 <- runIO $ TextIO.readFile =<< getDataFileName "data/Day01/example2.txt"
  puzzleInput <- runIO $ TextIO.readFile =<< getDataFileName "data/Day01/puzzle.txt"

  describe "Day 1" $ do
    describe "findDigits" $ do
      it "solves 1abc2" $ do
        Day01.findDigits1 "1abc2" `shouldBe` 12

      it "solves pqr3stu8vwx" $ do
        Day01.findDigits1 "pqr3stu8vwx" `shouldBe` 38

      it "solves a1b2c3d4e5f" $ do
        Day01.findDigits1 "a1b2c3d4e5f" `shouldBe` 15

      it "solves treb7uchet" $ do
        Day01.findDigits1 "treb7uchet" `shouldBe` 77

      it "solves the example input" $ do
        Day01.puzzle1 exampleInput1 `shouldBe` 142

    describe "puzzle1" $ do
      it "solves the puzzle input" $ do
        Day01.puzzle1 puzzleInput `shouldBe` 55130

    describe "findDigits2" $ do
      it "solves 1abc2" $ do
        Day01.findDigits2 "1abc2" `shouldBe` 12

      it "solves pqr3stu8vwx" $ do
        Day01.findDigits2 "pqr3stu8vwx" `shouldBe` 38

      it "solves a1b2c3d4e5f" $ do
        Day01.findDigits2 "a1b2c3d4e5f" `shouldBe` 15

      it "solves treb7uchet" $ do
        Day01.findDigits2 "treb7uchet" `shouldBe` 77

      it "solves two1nine" $ do
        Day01.findDigits2 "two1nine" `shouldBe` 29

      it "solves eightwothree" $ do
        Day01.findDigits2 "eightwothree" `shouldBe` 83

      it "solves abcone2threexyz" $ do
        Day01.findDigits2 "abcone2threexyz" `shouldBe` 13

      it "solves xtwone3four" $ do
        Day01.findDigits2 "xtwone3four" `shouldBe` 24

      it "solves 4nineeightseven2" $ do
        Day01.findDigits2 "4nineeightseven2" `shouldBe` 42

      it "solves zoneight234" $ do
        Day01.findDigits2 "zoneight234" `shouldBe` 14

      it "solves 7pqrstsixteen" $ do
        Day01.findDigits2 "7pqrstsixteen" `shouldBe` 76

      it "solves 19581" $ do
        Day01.findDigits2 "19581" `shouldBe` 11

      it "solves qp4" $ do
        Day01.findDigits2 "qp4" `shouldBe` 44

      it "solves eightwo" $ do
        Day01.findDigits2 "eightwo" `shouldBe` 82

    describe "puzzle2" $ do
      it "solves the example input" $ do
        Day01.puzzle2 exampleInput2 `shouldBe` 281

      it "solves the puzzle input" $ do
        Day01.puzzle2 puzzleInput `shouldBe` 54985
