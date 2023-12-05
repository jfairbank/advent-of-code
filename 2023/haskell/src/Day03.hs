{-# LANGUAGE TypeApplications #-}

module Day03 (puzzle1, puzzle2) where

import Control.Monad ((>=>))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Grid (Grid)
import qualified Grid

data Item = Digit Char | Symbol Char | Empty deriving (Show)

parseGrid :: Text -> Grid Item
parseGrid =
  Grid.fromList . fmap (fmap parseItem) . lines . Text.unpack . Text.strip
  where
    parseItem value
      | isDigit value = Digit value
      | value == '.' = Empty
      | otherwise = Symbol value

puzzle1 :: Text -> Int
puzzle1 input =
  sum partNumbers
  where
    grid = parseGrid input
    numbers = findNumbers grid

    touchesSymbol = maybe False (any isSymbol) . flip Grid.getNeighbors grid

    isSymbol (Digit _) = False
    isSymbol (Symbol _) = True
    isSymbol Empty = False

    partNumbers = read @Int . fmap snd <$> filter (any (touchesSymbol . fst)) numbers

    findNumbers =
      Grid.indexedFoldr
        ( \i row numbers ->
            uncurry consMaybe $
              Grid.indexedRowFoldr
                ( \j item (currentNumber, numbers') ->
                    case item of
                      Digit digit ->
                        ( Just $ ((i, j), digit) `consOntoMaybe` currentNumber,
                          numbers'
                        )
                      _ ->
                        ( Nothing,
                          consMaybe currentNumber numbers'
                        )
                )
                ( Nothing,
                  numbers
                )
                row
        )
        []

    consMaybe maybeValue list = maybe list (: list) maybeValue

    consOntoMaybe value = maybe [value] (value :)

puzzle2 :: Text -> Int
puzzle2 input =
  Grid.indexedFoldl
    ( \i ->
        Grid.indexedRowFoldr
          ( \j item acc' -> fromMaybe acc' $ do
              (partNumber1, partNumber2) <- getGearPartNumbers item (i, j)
              return $ acc' + partNumber1 * partNumber2
          )
    )
    0
    grid
  where
    grid = parseGrid input

    validateGearSymbol (Symbol '*') = Just ()
    validateGearSymbol _ = Nothing

    getGearPartNumbers item coord = do
      validateGearSymbol item
      neighbors <- Grid.getNeighborCoords coord grid

      let partNumberCoords = Set.toList $ Set.fromList $ mapMaybe (`getAsPartNumber` grid) neighbors

      case partNumberCoords of
        [(_, a), (_, b)] -> Just (a, b)
        _ -> Nothing

    getAsPartNumber coord@(i, j) grid = do
      char <- getAsDigit coord

      let right = fromMaybe [] $ toTheRight j
      let (newIndex, left) = fromMaybe (j, []) $ toTheLeft j

      return ((i, newIndex), read $ left ++ [char] ++ right)
      where
        toTheLeft index = do
          char <- getAsDigit (i, index - 1)
          let (newIndex, left) = fromMaybe (index - 1, []) $ toTheLeft (index - 1)
          return (newIndex, left ++ [char])

        toTheRight index = do
          char <- getAsDigit (i, index + 1)
          return $ maybe [char] (char :) $ toTheRight (index + 1)

        getAsDigit = flip Grid.get grid >=> asDigit

        asDigit (Digit char) = Just char
        asDigit _ = Nothing
