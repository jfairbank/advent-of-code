module Day06 (puzzle1, puzzle2) where

import Data.List (tails)
import qualified Data.Set as Set

puzzle1 :: String -> Int
puzzle1 =
  numCharsUntilEndOfMarker 4

puzzle2 :: String -> Int
puzzle2 =
  numCharsUntilEndOfMarker 14

numCharsUntilEndOfMarker :: Int -> String -> Int
numCharsUntilEndOfMarker markerSize input =
  length (takeWhile hasRepeatedChar candidates) + markerSize
  where
    candidates = filter ((== markerSize) . length) $ take markerSize <$> tails input

hasRepeatedChar :: String -> Bool
hasRepeatedChar string =
  Set.size (Set.fromList string) < length string
