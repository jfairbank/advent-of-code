{-# LANGUAGE TupleSections #-}

module Grid
  ( Grid,
    columnSize,
    findCoord,
    findCoords,
    fromList,
    get,
    getBottomLeftNeighbor,
    getBottomRightNeighbor,
    getNeighborCoords,
    getNeighbors,
    getPathsWithLength,
    getTopLeftNeighbor,
    getTopRightNeighbor,
    indexedFoldl,
    indexedFoldlThroughRows,
    indexedFoldr,
    indexedFoldrOverColumns,
    indexedRowFoldl,
    indexedRowFoldr,
    rowSize,
    rowToList,
  )
where

import Coord (Coord)
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)

data Row a = Row
  { size :: !Int,
    rowArray :: !(Array Int a)
  }
  deriving (Show)

data Grid_ a = Grid_
  { array :: !(Array Int a),
    rowSize :: !Int,
    columnSize :: !Int
  }
  deriving (Show)

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

getRelativeTo :: (Coord -> Coord) -> Coord -> Grid a -> Maybe a
getRelativeTo offsetFn coord grid = do
  validateCoord coord grid
  Grid.get (offsetFn coord) grid

getTopLeftNeighbor :: Coord -> Grid a -> Maybe a
getTopLeftNeighbor = getRelativeTo $ bimap (subtract 1) (subtract 1)

getTopRightNeighbor :: Coord -> Grid a -> Maybe a
getTopRightNeighbor = getRelativeTo $ bimap (subtract 1) (+ 1)

getBottomRightNeighbor :: Coord -> Grid a -> Maybe a
getBottomRightNeighbor = getRelativeTo $ bimap (+ 1) (+ 1)

getBottomLeftNeighbor :: Coord -> Grid a -> Maybe a
getBottomLeftNeighbor = getRelativeTo $ bimap (+ 1) (subtract 1)

getPathsWithLength :: Int -> Coord -> Grid a -> Maybe [[a]]
getPathsWithLength len coord@(i, j) grid = do
  validateCoord coord grid
  return $
    filter (\path -> length path == len) $
      mapMaybe (`Grid.get` grid)
        <$> [ up,
              up' `plus` right',
              right,
              down' `plus` right',
              down,
              down' `plus` left',
              left,
              up' `plus` left'
            ]
  where
    offset = len - 1

    up' = [i, i - 1 .. i - offset]
    right' = [j .. j + offset]
    down' = [i .. i + offset]
    left' = [j, j - 1 .. j - offset]
    up = up' <&> (,j)
    right = (i,) <$> right'
    down = down' <&> (,j)
    left = (i,) <$> left'
    dir1 `plus` dir2 = zip dir1 dir2

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

indexedFoldrOverColumns :: (Int -> [a] -> b -> b) -> b -> Grid a -> b
indexedFoldrOverColumns f init grid@Grid_ {columnSize = columnSize, rowSize = rowSize} =
  foldr
    ( \j acc ->
        maybe acc (\element -> f j element acc) $
          mapM (flip get grid . (,j)) [0 .. rowSize - 1]
    )
    init
    [0 .. columnSize - 1]

indexedFoldl :: (Int -> b -> Row a -> b) -> b -> Grid a -> b
indexedFoldl f init =
  snd
    . foldl
      (\(i, acc) row -> (i + 1, f i acc row))
      (0, init)

indexedFoldlThroughRows :: (Coord -> b -> a -> b) -> b -> Grid a -> b
indexedFoldlThroughRows f =
  Grid.indexedFoldl (\i -> Grid.indexedRowFoldl (\j -> f (i, j)))

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

findIndex :: (a -> Bool) -> Int -> Array Int a -> Maybe Int
findIndex f size array =
  helper 0
  where
    helper i
      | i >= size = Nothing
      | f (array ! i) = Just i
      | otherwise = helper (i + 1)

findCoord :: (a -> Bool) -> Grid a -> Maybe Coord
findCoord f grid@(Grid_ {array = array, columnSize = columnSize, rowSize = rowSize}) =
  helper 0
  where
    helper rowIndex
      | rowIndex >= rowSize = Nothing
      | otherwise =
          case findIndex f columnSize $ rowArray $ array ! rowIndex of
            Just columnIndex -> Just (rowIndex, columnIndex)
            Nothing -> helper (rowIndex + 1)

findCoords :: (a -> Bool) -> Grid a -> [Coord]
findCoords f =
  indexedFoldr
    ( \i row acc ->
        indexedRowFoldr
          ( \j element acc' ->
              if f element then (i, j) : acc' else acc'
          )
          acc
          row
    )
    []

rowToList :: Row a -> [a]
rowToList = Array.elems . rowArray
