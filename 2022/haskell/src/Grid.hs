module Grid
  ( Grid,
    Key,
    fromList,
    fromListMap,
    get,
    lookup,
    size,
    toIndexedList,
  )
where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Bifunctor (bimap)
import qualified Range
import qualified Utils.List
import Prelude hiding (lookup)

type Key = (Int, Int)

type Grid a = Array Key a

fromList :: [[a]] -> Grid a
fromList =
  fromListMap id

fromListMap :: (a -> b) -> [[a]] -> Grid b
fromListMap f matrix =
  Array.array ((0, 0), (numRows - 1, numColumns - 1)) flattenedList
  where
    numRows = length matrix
    numColumns = length $ head matrix
    flattenedList =
      Utils.List.indexedConcatMap
        (\i -> Utils.List.indexedMap (\j item -> ((i, j), f item)))
        matrix

get :: (Int, Int) -> Grid a -> a
get = flip (!)

toIndexedList :: Grid a -> [((Int, Int), a)]
toIndexedList = Array.assocs

lookup :: (Int, Int) -> Grid a -> Maybe a
lookup (i, j) grid =
  if Range.contains i yRange && Range.contains j xRange
    then Just $ get (i, j) grid
    else Nothing
  where
    ((y1, x1), (y2, x2)) = Array.bounds grid
    yRange = Range.new y1 y2
    xRange = Range.new x1 x2

size :: Grid a -> (Int, Int)
size =
  bimap (+ 1) (+ 1) . snd . Array.bounds
