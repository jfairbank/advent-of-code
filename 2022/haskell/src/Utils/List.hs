module Utils.List (indexedConcatMap, indexedMap, toPair) where

indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f =
  reverse
    . snd
    . foldl
      ( \(i, mapped) item ->
          (i + 1, f i item : mapped)
      )
      (0, [])

indexedConcatMap :: (Int -> a -> [b]) -> [a] -> [b]
indexedConcatMap f =
  concat
    . reverse
    . snd
    . foldl
      ( \(i, mapped) item ->
          (i + 1, f i item : mapped)
      )
      (0, [])

toPair :: [a] -> Maybe (a, a)
toPair (x : y : _) = Just (x, y)
toPair _ = Nothing
