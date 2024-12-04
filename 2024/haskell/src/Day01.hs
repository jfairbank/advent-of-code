{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Day01
  ( puzzle1,
    puzzle2,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

newtype Location = Location {locationValue :: Int} deriving (Eq, Num, Ord)

type Tallies = Map Location Int

listToPair :: [a] -> Maybe (a, a)
listToPair (a : b : _) = Just (a, b)
listToPair _ = Nothing

diff :: (Num a) => (a, a) -> a
diff (x, y) = abs $ x - y

parsePair :: Text -> Maybe (Location, Location)
parsePair = listToPair . mapMaybe (fmap Location . readMaybe @Int . Text.unpack) . Text.words

parseLists :: Text -> ([Location], [Location])
parseLists = unzip . mapMaybe parsePair . Text.lines

sortLists :: (Ord a) => ([a], [a]) -> ([a], [a])
sortLists = bimap sort sort

pairUp :: ([a], [a]) -> [(a, a)]
pairUp = uncurry zip

tally :: [Location] -> Tallies
tally =
  foldl (flip (Map.alter (\count -> (1 +) <$> count <|> Just 1))) Map.empty

computeSimilarities :: Tallies -> [Location] -> [Int]
computeSimilarities tallies =
  mapMaybe $ \location ->
    (locationValue location *) <$> Map.lookup location tallies

puzzle1 :: Text -> Int
puzzle1 =
  locationValue . sum . fmap diff . pairUp . sortLists . parseLists

puzzle2 :: Text -> Int
puzzle2 input =
  sum $ computeSimilarities tallies list1
  where
    (list1, list2) = parseLists input
    tallies = tally list2
