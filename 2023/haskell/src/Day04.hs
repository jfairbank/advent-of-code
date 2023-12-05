module Day04 (puzzle1, puzzle2) where

import Control.Monad (void)
import Data.Either.Extra (eitherToMaybe)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (digit, many1, manyTill, parse, sepEndBy1, string, try)
import Text.Parsec.String (Parser)

int :: Parser Int
int = read <$> many1 digit

space :: Parser ()
space = void $ many1 $ string " "

data Card = Card {cardId :: Int, winning :: Set Int, available :: Set Int}

cardParser :: Parser Card
cardParser = do
  string "Card"
  space
  cardId <- int
  string ":"
  winning <- manyTill (space >> int) $ try $ string " |"
  space
  available <- sepEndBy1 int space
  return $ Card cardId (Set.fromList winning) (Set.fromList available)

parseCards :: Text -> [Card]
parseCards =
  mapMaybe (eitherToMaybe . parse cardParser "") . lines . Text.unpack . Text.strip

numMatching :: Card -> Maybe Int
numMatching card =
  let result = Set.size (Set.intersection (winning card) (available card))
   in if result > 0 then Just result else Nothing

puzzle1 :: Text -> Int
puzzle1 input =
  sum [maybe 0 (\size -> 2 ^ (size - 1)) $ numMatching card | card <- parseCards input]

puzzle2 :: Text -> Int
puzzle2 input =
  sum $
    foldl
      ( \acc card@Card {cardId = cardId} -> fromMaybe acc $ do
          size <- numMatching card
          let currentCardCount = acc ! cardId
          return $ foldr (Map.adjust (+ currentCardCount)) acc [cardId + 1 .. cardId + size]
      )
      (Map.fromAscList [(cardId c, 1) | c <- cards])
      cards
  where
    cards = parseCards input
