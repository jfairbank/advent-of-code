{-# LANGUAGE BangPatterns #-}

module Day04
  ( puzzle1,
    puzzle2,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Grid (Grid)
import qualified Grid

parseGrid :: String -> Grid Char
parseGrid =
  Grid.fromList . lines

countOccurrences :: (a -> Bool) -> [a] -> Int
countOccurrences fn =
  countOccurrences' 0
  where
    countOccurrences' !count [] = count
    countOccurrences' !count (x : xs)
      | fn x = countOccurrences' (count + 1) xs
      | otherwise = countOccurrences' count xs

puzzle1 :: Text -> Int
puzzle1 input =
  Grid.indexedFoldlThroughRows
    ( \coord count char ->
        let paths = fromMaybe [] $ Grid.getPathsWithLength 4 coord grid
         in count + (if char == 'X' then countOccurrences isXmasPath paths else 0)
    )
    0
    grid
  where
    grid = parseGrid $ Text.unpack input
    isXmasPath path = path == "XMAS" || path == "SAMX"

puzzle2 :: Text -> Int
puzzle2 input =
  Grid.indexedFoldlThroughRows
    ( \coord count char ->
        let haveXShape = char == 'A' && all (maybe False isMasPath . sequence) (getDiagonalPaths coord char)
         in count + (if haveXShape then 1 else 0)
    )
    0
    grid
  where
    getTopLeftNeighbor = (`Grid.getTopLeftNeighbor` grid)
    getTopRightNeighbor = (`Grid.getTopRightNeighbor` grid)
    getBottomLeftNeighbor = (`Grid.getBottomLeftNeighbor` grid)
    getBottomRightNeighbor = (`Grid.getBottomRightNeighbor` grid)

    getDiagonalPaths coord char =
      [ [getTopLeftNeighbor coord, Just char, getBottomRightNeighbor coord],
        [getBottomLeftNeighbor coord, Just char, getTopRightNeighbor coord]
      ]

    grid = parseGrid $ Text.unpack input
    isMasPath path = path == "MAS" || path == "SAM"
