module Day09 (puzzle1, puzzle2) where

import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Parsec (char, choice, digit, many, many1, newline, parse, sepEndBy1)

puzzle :: ([Int] -> Int -> Int) -> Text -> Int
puzzle applyDiff input =
  sum $ foldr applyDiff 0 . diffsUntilSame <$> parseValues input
  where
    parseValues = fromMaybe [] . eitherToMaybe . parse pointsParser ""
    pointsParser = sepEndBy1 (sepEndBy1 int $ many1 $ char ' ') newline
    int = fmap read $ (:) <$> choice [char '-', digit] <*> many digit

    diffsUntilSame = takeWhile (not . all (== 0)) . iterate computeDiffs
    computeDiffs ys = zipWith (-) (tail ys) ys

puzzle1 :: Text -> Int
puzzle1 = puzzle ((+) . last)

puzzle2 :: Text -> Int
puzzle2 = puzzle ((-) . head)
