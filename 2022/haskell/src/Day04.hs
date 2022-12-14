{-# LANGUAGE OverloadedStrings #-}

module Day04 (puzzle1, puzzle2) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Range (Range)
import qualified Range
import qualified Utils
import qualified Utils.List

type Pair = (Range Int, Range Int)

puzzle1 :: Text -> Int
puzzle1 =
  countContainedRanges rangesHaveFullContainment . parseInput
  where
    rangesHaveFullContainment (range1, range2) =
      Range.isSubsetOf range1 range2 || Range.isSubsetOf range2 range1

puzzle2 :: Text -> Int
puzzle2 =
  countContainedRanges rangesOverlap . parseInput
  where
    rangesOverlap (range1, range2) =
      not $ Range.disjoint range1 range2

parseInput :: Text -> [Pair]
parseInput =
  fromMaybe [] . mapM parsePair . stripInput
  where
    stripInput = fmap Text.strip . Text.lines . Text.strip

countContainedRanges :: (Pair -> Bool) -> [Pair] -> Int
countContainedRanges checkContainment =
  getSum . foldMap (Sum . countContainedRange)
  where
    countContainedRange pair =
      if checkContainment pair then 1 else 0

parsePair :: Text -> Maybe Pair
parsePair pair =
  mapM parseRange (Text.split (== ',') pair)
    >>= Utils.List.toPair
  where
    parseRange range =
      mapM Utils.textReadMaybe (Text.split (== '-') range)
        >>= Utils.List.toPair
        <&> Range.fromTuple
