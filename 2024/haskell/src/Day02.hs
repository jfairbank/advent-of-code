module Day02
  ( puzzle1,
    puzzle2,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

data Direction = Increasing | Decreasing deriving (Eq, Show)

delete :: Int -> [a] -> [a]
delete _ [] = []
delete 0 (x : xs) = xs
delete index (x : xs) = x : delete (index - 1) xs

computeDirection :: (Int, Int) -> Maybe Direction
computeDirection (a, b)
  | diff `elem` boundaries = Just Decreasing
  | negate diff `elem` boundaries = Just Increasing
  | otherwise = Nothing
  where
    diff = a - b
    boundaries = [1 .. 3]

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [_] = True
isSafe (x : y : rest) =
  case computeDirection (x, y) of
    Just direction -> isSafeWithDirection direction (y : rest)
    Nothing -> False

isSafeWithDirection :: Direction -> [Int] -> Bool
isSafeWithDirection _ [] = True
isSafeWithDirection _ [_] = True
isSafeWithDirection direction (x : y : rest) =
  case computeDirection (x, y) of
    Just direction'
      | direction' == direction -> isSafeWithDirection direction (y : rest)
      | otherwise -> False
    Nothing ->
      False

oneLevelRemovedPermutations :: [Int] -> [[Int]]
oneLevelRemovedPermutations list =
  fmap (`delete` list) [0 .. length list - 1]

parseLine :: Text -> [Int]
parseLine =
  mapMaybe (readMaybe . Text.unpack) . Text.words

puzzle1 :: Text -> Int
puzzle1 input =
  length $ filter isSafe $ parseLine <$> Text.lines input

puzzle2 :: Text -> Int
puzzle2 input =
  length
    $ filter
      ( \list ->
          isSafe list || any isSafe (oneLevelRemovedPermutations list)
      )
    $ parseLine <$> Text.lines input
