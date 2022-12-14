{-# LANGUAGE TupleSections #-}

module Day08 (puzzle1, puzzle2) where

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Grid

type Grid = Grid.Grid Height

newtype Height = Height Int deriving (Eq, Ord)

instance Show Height where
  show (Height amount) = show amount

instance Read Height where
  readsPrec _ n = [(Height (read n :: Int), "")]

puzzle1 :: Text -> Int
puzzle1 input =
  length $ filter (flip isVisible grid . fst) $ Grid.toIndexedList grid
  where
    grid = parseInput $ Text.unpack $ Text.strip input

puzzle2 :: Text -> Int
puzzle2 input =
  maximum $ flip scenicScore grid . fst <$> Grid.toIndexedList grid
  where
    grid = parseInput $ Text.unpack $ Text.strip input

isVisible :: Grid.Key -> Grid -> Bool
isVisible key grid =
  isOnEdge key grid || any isVisibleFromDirection (neighbors key grid)
  where
    treeHeight = Grid.get key grid
    isVisibleFromDirection = all (< treeHeight)

isOnEdge :: Grid.Key -> Grid -> Bool
isOnEdge (i, j) grid =
  isOnTopEdge || isOnRightEdge || isOnBottomEdge || isOnLeftEdge
  where
    (numRows, numColumns) = Grid.size grid
    isOnTopEdge = i == 0
    isOnBottomEdge = i == numRows - 1
    isOnLeftEdge = j == 0
    isOnRightEdge = j == numColumns - 1

neighbors :: Grid.Key -> Grid -> [[Height]]
neighbors (i, j) grid =
  [topNeighbors, rightNeighbors, bottomNeighbors, leftNeighbors]
  where
    (numRows, numColumns) = Grid.size grid

    getVerticalNeighbors = mapMaybe $ flip Grid.lookup grid . (,j)
    getHorizontalNeighbors = mapMaybe $ flip Grid.lookup grid . (i,)

    topNeighbors = getVerticalNeighbors [i - 1, i - 2 .. 0]
    rightNeighbors = getHorizontalNeighbors [j + 1 .. numColumns - 1]
    bottomNeighbors = getVerticalNeighbors [i + 1 .. numRows - 1]
    leftNeighbors = getHorizontalNeighbors [j - 1, j - 2 .. 0]

scenicScore :: Grid.Key -> Grid -> Int
scenicScore key grid =
  product $ scoreForDirection 0 <$> neighbors key grid
  where
    treeHeight = Grid.get key grid
    scoreForDirection acc [] = acc
    scoreForDirection acc (neighbor : directionalNeighbors)
      | neighbor >= treeHeight = acc + 1
      | otherwise = scoreForDirection (acc + 1) directionalNeighbors

parseInput :: String -> Grid
parseInput input =
  Grid.fromListMap (read . List.singleton) $ lines input
