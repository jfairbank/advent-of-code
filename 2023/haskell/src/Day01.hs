module Day01
  ( findDigits1,
    findDigits2,
    puzzle1,
    puzzle2,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Either.Extra (eitherToMaybe)
import Data.List (find, isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec
  ( Parsec,
    anyChar,
    digit,
    getInput,
    getState,
    many1,
    parserFail,
    putState,
    runParser,
    setInput,
    string,
  )
import Text.Read (readMaybe)

safeFirst :: [a] -> Maybe a
safeFirst = listToMaybe

safeLast :: [a] -> Maybe a
safeLast = listToMaybe . reverse

findDigits :: (String -> String) -> String -> Int
findDigits parseInput input = fromMaybe 0 $ do
  first_ <- safeFirst candidates
  last_ <- safeLast candidates
  readMaybe [first_, last_]
  where
    candidates = parseInput input

puzzle :: (String -> Int) -> Text -> Int
puzzle findDigits_ input =
  sum $ findDigits_ <$> lines (Text.unpack input)

findDigits1 :: String -> Int
findDigits1 = findDigits (filter isDigit)

puzzle1 :: Text -> Int
puzzle1 = puzzle findDigits1

type InnerParser = Parsec String (Maybe Char) (Maybe Char)

parser :: Parsec String (Maybe Char) String
parser =
  catMaybes <$> many1 (wordParser <|> digitParser <|> fallbackParser)
  where
    wordParser :: InnerParser
    wordParser = do
      maybeLastChar <- getState
      currentInput <- getInput

      setInput $ maybe currentInput (: currentInput) maybeLastChar

      baseWordParser <|> do
        setInput currentInput
        baseWordParser

    baseWordParser :: InnerParser
    baseWordParser = do
      input <- getInput
      case findNumberWord input of
        Just (word, returnDigit) -> do
          string word
          putState $ safeLast word
          return $ Just returnDigit
        Nothing -> parserFail "No matching word found"

    findNumberWord :: String -> Maybe (String, Char)
    findNumberWord input =
      find
        (flip isPrefixOf input . fst)
        [ ("one", '1'),
          ("two", '2'),
          ("three", '3'),
          ("four", '4'),
          ("five", '5'),
          ("six", '6'),
          ("seven", '7'),
          ("eight", '8'),
          ("nine", '9')
        ]

    fallbackParser :: InnerParser
    fallbackParser = anyChar >> return Nothing

    digitParser :: InnerParser
    digitParser = Just <$> digit

findDigits2 :: String -> Int
findDigits2 =
  findDigits $ fromMaybe [] . eitherToMaybe . runParser parser Nothing ""

puzzle2 :: Text -> Int
puzzle2 =
  puzzle findDigits2
