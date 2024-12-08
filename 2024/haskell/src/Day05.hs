module Day05 (puzzle1, puzzle2) where

import Control.Applicative ((<|>))
import Data.Either (fromRight)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (digit, many1, newline, parse, sepBy1, sepEndBy1, string)
import Text.Parsec.String (Parser)

type X = Int

type Y = Int

data Rule = Rule !X !Y deriving (Show)

type RulesMap = Map X (Set Y)

type Instruction = [Int]

sortInstruction :: RulesMap -> Instruction -> Instruction
sortInstruction rulesMap =
  List.sortBy
    ( \a b ->
        if maybe False (Set.member b) (Map.lookup a rulesMap)
          then LT
          else GT
    )

isValidInstruction :: RulesMap -> Instruction -> Bool
isValidInstruction _ [_] = True
isValidInstruction _ [] = True
isValidInstruction rulesMap (x : ys) =
  case Map.lookup x rulesMap of
    Just allowedSubsequentPages ->
      Set.fromList ys `Set.isSubsetOf` allowedSubsequentPages && isValidInstruction rulesMap ys
    Nothing ->
      False

intParser :: Parser Int
intParser = read <$> many1 digit

ruleParser :: Parser Rule
ruleParser = Rule <$> intParser <* string "|" <*> intParser

instructionParser :: Parser Instruction
instructionParser = sepBy1 intParser $ string ","

parser :: Parser ([Rule], [Instruction])
parser = do
  rules <- sepEndBy1 ruleParser newline
  newline
  instructions <- sepEndBy1 instructionParser newline
  return (rules, instructions)

buildRulesMap :: [Rule] -> RulesMap
buildRulesMap =
  foldl
    ( \map (Rule x y) ->
        Map.alter
          ( \maybeSet ->
              (Set.insert y <$> maybeSet) <|> Just (Set.singleton y)
          )
          x
          map
    )
    Map.empty

middle :: [a] -> Maybe a
middle [] = Nothing
middle list@(_ : _ : _ : _) = middle (tail (init list))
middle list = listToMaybe list

puzzle1 :: Text -> Int
puzzle1 input = fromRight 0 $ do
  (rules, instructions) <- parse parser "" $ Text.unpack $ Text.strip input

  let rulesMap = buildRulesMap rules
  let validInstructions = filter (isValidInstruction rulesMap) instructions

  return $ sum $ mapMaybe middle validInstructions

puzzle2 :: Text -> Int
puzzle2 input = fromRight 0 $ do
  (rules, instructions) <- parse parser "" $ Text.unpack $ Text.strip input

  let rulesMap = buildRulesMap rules
  let invalidInstructions = filter (not . isValidInstruction rulesMap) instructions

  return $ sum $ mapMaybe (middle . sortInstruction rulesMap) invalidInstructions
