{-# LANGUAGE PatternSynonyms #-}

module Day08 (puzzle1, puzzle2) where

import Data.Either.Extra (eitherToMaybe)
import Data.Functor (($>))
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Parsec (choice, letter, many1, newline, parse, sepEndBy1, string)
import Text.Parsec.Text (Parser)

data Direction = L | R

type Key = String

pattern KeyEndsWith :: Char -> Key
pattern KeyEndsWith x <- [_, _, x]

data Mapping = Mapping {mappingKey :: Key, mappingLeft :: Key, mappingRight :: Key}

type Mappings = Map Key Mapping

type DesertMap = ([Direction], Mappings)

directionParser :: Parser Direction
directionParser = choice [string "L" $> L, string "R" $> R]

mappingsParser :: Parser Mappings
mappingsParser = foldr addMapping Map.empty <$> sepEndBy1 mappingParser newline
  where
    addMapping mapping = Map.insert (mappingKey mapping) mapping
    key = many1 letter
    mappingParser =
      Mapping <$> (key <* string " = (") <*> (key <* string ", ") <*> (key <* string ")")

parseDesertMap :: Text -> DesertMap
parseDesertMap = fromMaybe ([], Map.empty) . eitherToMaybe . parse desertMapParser ""
  where
    desertMapParser = (,) <$> (many1 directionParser <* many1 newline) <*> mappingsParser

findNeededSteps :: DesertMap -> Key -> Int
findNeededSteps (initialDirections, mappings) = helper (cycle initialDirections) 0
  where
    getMapping = (mappings !)

    followDirection L = mappingLeft
    followDirection R = mappingRight

    helper _ step (KeyEndsWith 'Z') = step
    helper (direction : directions) step key' =
      helper directions (step + 1) (followDirection direction $ getMapping key')

puzzle1 :: Text -> Int
puzzle1 input =
  findNeededSteps (parseDesertMap input) "AAA"

puzzle2 :: Text -> Int
puzzle2 input =
  foldr (lcm . findNeededSteps desertMap) 1 nodesEndingInA
  where
    desertMap@(_, mappings) = parseDesertMap input
    nodesEndingInA = [key | key@(KeyEndsWith 'A') <- Map.keys mappings]
