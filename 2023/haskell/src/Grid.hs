module Grid
  ( Coord,
    Grid,
    columnSize,
    fromList,
    get,
    getNeighborCoords,
    getNeighbors,
    indexedFoldl,
    indexedFoldr,
    indexedRowFoldl,
    indexedRowFoldr,
    rowSize,
  )
where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)

data Row a = Row
  { size :: Int,
    rowArray :: Array Int a
  }

data Grid_ a = Grid_
  { array :: Array Int a,
    rowSize :: Int,
    columnSize :: Int
  }

type Grid a = Grid_ (Row a)

fromList :: [[a]] -> Grid a
fromList [] = Grid_ {array = Array.listArray (1, 0) [], rowSize = 0, columnSize = 0}
fromList rows =
  Grid_
    { array = Array.listArray outerBounds $ fmap (Row columnSize . Array.listArray innerBounds) rows,
      rowSize = rowSize,
      columnSize = columnSize
    }
  where
    outerBounds = (0, rowSize - 1)
    innerBounds = (0, columnSize - 1)
    columnSize = length $ head rows
    rowSize = length rows

get :: Coord -> Grid a -> Maybe a
get coord@(i, j) grid@(Grid_ {array = array}) = do
  validateCoord coord grid
  return $ rowArray (array ! i) ! j

validateCoord :: Coord -> Grid a -> Maybe Coord
validateCoord coord@(i, j) Grid_ {array = array, rowSize = rowSize, columnSize = columnSize} =
  if i < 0 || i >= rowSize || j < 0 || j >= columnSize then Nothing else Just coord

getNeighborCoords :: Coord -> Grid a -> Maybe [Coord]
getNeighborCoords coord@(i, j) grid = do
  validateCoord coord grid
  return $
    mapMaybe
      (`validateCoord` grid)
      [ (i - 1, j - 1),
        (i - 1, j),
        (i - 1, j + 1),
        (i, j - 1),
        (i, j),
        (i, j + 1),
        (i + 1, j - 1),
        (i + 1, j),
        (i + 1, j + 1)
      ]

getNeighbors :: Coord -> Grid a -> Maybe [a]
getNeighbors coord@(i, j) grid = do
  validateCoord coord grid

  return $
    mapMaybe
      (`get` grid)
      [ (i - 1, j - 1),
        (i - 1, j),
        (i - 1, j + 1),
        (i, j - 1),
        (i, j),
        (i, j + 1),
        (i + 1, j - 1),
        (i + 1, j),
        (i + 1, j + 1)
      ]

instance Foldable Grid_ where
  foldr f init (Grid_ {array = array}) = foldr f init array

instance Foldable Row where
  foldr f init (Row {rowArray = array}) = foldr f init array

indexedFoldr :: (Int -> Row a -> b -> b) -> b -> Grid a -> b
indexedFoldr f init grid =
  snd $
    foldr
      (\row (i, acc) -> (i - 1, f i row acc))
      (rowSize grid - 1, init)
      grid

indexedFoldl :: (Int -> b -> Row a -> b) -> b -> Grid a -> b
indexedFoldl f init =
  snd
    . foldl
      (\(i, acc) row -> (i + 1, f i acc row))
      (0, init)

indexedRowFoldr :: (Int -> a -> b -> b) -> b -> Row a -> b
indexedRowFoldr f init row@Row {size = size} =
  snd $
    foldr
      (\item (j, acc) -> (j - 1, f j item acc))
      (size - 1, init)
      row

indexedRowFoldl :: (Int -> b -> a -> b) -> b -> Row a -> b
indexedRowFoldl f init =
  snd
    . foldl
      (\(j, acc) item -> (j + 1, f j acc item))
      (0, init)
