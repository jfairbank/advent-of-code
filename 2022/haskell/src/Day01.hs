{-# LANGUAGE OverloadedStrings #-}

module Day01 (puzzle1, puzzle2) where

import Control.Arrow ((>>>))
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Text (Text)
import qualified Data.Text as Text

puzzle1 :: Text -> Int
puzzle1 =
  groupAndCountCalories >>> List.maximum

puzzle2 :: Text -> Int
puzzle2 =
  groupAndCountCalories
    >>> List.sortBy (flip compare)
    >>> take 3
    >>> sum

groupAndCountCalories :: Text -> [Int]
groupAndCountCalories =
  Text.lines
    >>> fmap Text.strip
    >>> Split.splitOn [""]
    >>> fmap countCalories

countCalories :: [Text] -> Int
countCalories =
  sum . fmap (read . Text.unpack)
