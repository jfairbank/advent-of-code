module Range
  ( Range,
    contains,
    disjoint,
    fromTuple,
    isSubsetOf,
    new,
    toList,
  )
where

data Range a = Range a a

new :: a -> a -> Range a
new = Range

fromTuple :: (a, a) -> Range a
fromTuple = uncurry Range

contains :: (Enum a, Ord a) => a -> Range a -> Bool
contains value range =
  value `elem` toList range

toList :: Enum a => Range a -> [a]
toList (Range from to) = [from .. to]

disjoint :: Ord a => Range a -> Range a -> Bool
disjoint (Range from1 to1) (Range from2 to2) =
  to1 < from2 || to2 < from1

isSubsetOf :: Ord a => Range a -> Range a -> Bool
isSubsetOf (Range from1 to1) (Range from2 to2) =
  from2 <= from1 && to1 <= to2
