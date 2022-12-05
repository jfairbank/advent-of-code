{-# LANGUAGE OverloadedStrings #-}

module Day04 (puzzle1, puzzle2) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Utils

type Range = Set Int

type Pair = (Range, Range)

puzzle1 :: Text -> Int
puzzle1 =
  countContainedRanges countFullyContainedRange . parseInput

puzzle2 :: Text -> Int
puzzle2 =
  countContainedRanges countPartiallyContainedRange . parseInput

parseInput :: Text -> [Pair]
parseInput =
  fromMaybe [] . mapM parsePair . stripInput

stripInput :: Text -> [Text]
stripInput =
  fmap Text.strip . Text.lines . Text.strip

countContainedRanges :: (Pair -> Int) -> [Pair] -> Int
countContainedRanges counter =
  getSum . foldMap (Sum . counter)

countFullyContainedRange :: Pair -> Int
countFullyContainedRange (range1, range2) =
  if Set.isSubsetOf range1 range2 || Set.isSubsetOf range2 range1 then 1 else 0

countPartiallyContainedRange :: Pair -> Int
countPartiallyContainedRange (range1, range2) =
  if Set.size (Set.intersection range1 range2) > 0 then 1 else 0

parsePair :: Text -> Maybe Pair
parsePair pair =
  mapM parseRange (Text.split (== ',') pair)
    >>= Utils.listToPair

parseRange :: Text -> Maybe Range
parseRange range =
  mapM Utils.textReadMaybe (Text.split (== '-') range)
    >>= Utils.listToPair
    <&> Set.fromList . uncurry enumFromTo
