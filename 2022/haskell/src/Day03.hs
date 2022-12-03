module Day03 (puzzle1, puzzle2) where

import Data.Bifunctor (second)
import Data.Char (ord)
import qualified Data.List.Split as Split
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple.Extra (dupe)

puzzle1 :: Text -> Int
puzzle1 input =
  sum $ processLine <$> stripInput input
  where
    processLine :: String -> Int
    processLine line =
      processGroup $ Split.chunksOf (length line `div` 2) line

puzzle2 :: Text -> Int
puzzle2 input =
  sum $ processGroup <$> Split.chunksOf 3 (stripInput input)

processGroup :: [String] -> Int
processGroup group =
  prioritize $ Set.findMin (foldl1 Set.intersection $ Set.fromList <$> group)

stripInput :: Text -> [String]
stripInput =
  fmap (Text.unpack . Text.strip) . Text.lines . Text.strip

priorityMap :: Map Char Int
priorityMap =
  Map.fromList $ lowercase ++ uppercase
  where
    lowercase = buildAssocList (subtract 96) ['a' .. 'z']
    uppercase = buildAssocList (subtract 38) ['A' .. 'Z']
    buildAssocList adjustValue list = second (adjustValue . ord) . dupe <$> list

prioritize :: Char -> Int
prioritize =
  (priorityMap !)
