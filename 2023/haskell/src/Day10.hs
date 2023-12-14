{-# LANGUAGE TupleSections #-}

module Day10 (puzzle1, puzzle2) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (guard, (<=<))
import Coord (Coord)
import qualified Coord
import qualified Data.Bifunctor as Bifunctor
import Data.Either.Extra (eitherToMaybe)
import Data.Functor (($>), (<&>))
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Grid (Grid)
import qualified Grid
import Text.Parsec (char, choice, many1, newline, parse, sepEndBy1)
import Text.Parsec.Text (Parser)

data Pipe = Vertical | Horizontal | NEBend | NWBend | SWBend | SEBend | Start deriving (Eq, Ord)

pipeParser :: Parser Pipe
pipeParser =
  choice
    [ char '|' $> Vertical,
      char '-' $> Horizontal,
      char 'L' $> NEBend,
      char 'J' $> NWBend,
      char '7' $> SWBend,
      char 'F' $> SEBend,
      char 'S' $> Start
    ]

data Item = Ground | Pipe Pipe deriving (Eq, Ord)

asPipe :: Item -> Maybe Pipe
asPipe (Pipe pipe) = Just pipe
asPipe _ = Nothing

itemParser :: Parser Item
itemParser =
  (char '.' $> Ground) <|> (Pipe <$> pipeParser)

type ItemGrid = Grid Item

type CoordPair a = (Coord, a)

type Connection = (Int, Int)

itemGridParser :: Parser ItemGrid
itemGridParser = Grid.fromList <$> sepEndBy1 (many1 itemParser) newline

parseItemGrid :: Text -> Maybe ItemGrid
parseItemGrid = eitherToMaybe . parse itemGridParser ""

getPipe :: Coord -> ItemGrid -> Maybe Pipe
getPipe coord = asPipe <=< Grid.get coord

possibleConnections :: Pipe -> [Connection]
possibleConnections Vertical = [(-1, 0), (1, 0)]
possibleConnections Horizontal = [(0, -1), (0, 1)]
possibleConnections NEBend = [(-1, 0), (0, 1)]
possibleConnections NWBend = [(-1, 0), (0, -1)]
possibleConnections SWBend = [(1, 0), (0, -1)]
possibleConnections SEBend = [(1, 0), (0, 1)]
possibleConnections Start = [(-1, 0), (0, 1), (1, 0), (0, -1)]

validPossibleConnections :: CoordPair Pipe -> ItemGrid -> [(Connection, Pipe)]
validPossibleConnections (coord, pipe) grid =
  mapMaybe
    ( \connection ->
        getPipe (Coord.add coord connection) grid
          >>= validateConnection connection
    )
    (possibleConnections pipe)

validateConnection :: Connection -> Pipe -> Maybe (Connection, Pipe)
validateConnection connection connector =
  guard (Coord.negate connection `elem` possibleConnections connector)
    >> pure (connection, connector)

startActsAs :: ItemGrid -> CoordPair Pipe -> Maybe Pipe
startActsAs grid coordPair@(_, Start) =
  fst <$> find (uncurry $ const $ all (`elem` (fst <$> startConnections))) pipeConnections
  where
    startConnections = validPossibleConnections coordPair grid
    pipeConnections = [Vertical, Horizontal, NEBend, NWBend, SWBend, SEBend] <&> id &&& possibleConnections
startActsAs _ _ = Nothing

pipeNeighbors :: CoordPair Pipe -> ItemGrid -> [CoordPair Pipe]
pipeNeighbors coordPair@(coord, pipe) grid =
  Bifunctor.first (Coord.add coord) <$> validPossibleConnections coordPair grid

firstPipeNeighbor :: CoordPair Pipe -> ItemGrid -> Maybe (CoordPair Pipe)
firstPipeNeighbor coordPair = listToMaybe . pipeNeighbors coordPair

nextPipeNeighbor :: CoordPair Pipe -> Set (CoordPair Pipe) -> ItemGrid -> Maybe (CoordPair Pipe)
nextPipeNeighbor coordPair visited grid =
  listToMaybe unvisitedNeighbors
  where
    neighbors = Set.fromList $ pipeNeighbors coordPair grid
    unvisitedNeighbors = Set.toList $ Set.difference neighbors visited

findStart :: ItemGrid -> Maybe Coord
findStart = Grid.findCoord (== Pipe Start)

findLoop :: ItemGrid -> Maybe (Set (CoordPair Pipe))
findLoop grid = do
  startCoordPair <- findStart grid <&> (,Start)

  let initialNeighbor = firstPipeNeighbor startCoordPair grid
  let initialVisited = Set.singleton startCoordPair

  return $ helper initialNeighbor grid initialVisited
  where
    helper Nothing _ visited = visited
    helper (Just coordPair@(coord, pipe)) grid visited =
      helper (nextPipeNeighbor coordPair visited grid) grid (Set.insert coordPair visited)

data Containment = Inside | Outside deriving (Eq)

flipContainment :: Containment -> Containment
flipContainment Inside = Outside
flipContainment Outside = Inside

isInside :: Containment -> Bool
isInside containment = containment == Inside

puzzle1 :: Text -> Int
puzzle1 input = fromMaybe 0 $ do
  grid <- parseItemGrid input
  loop <- findLoop grid
  return $ Set.size loop `div` 2

puzzle2 :: Text -> Int
puzzle2 input = fromMaybe 0 $ do
  grid <- parseItemGrid input
  loop <- findLoop grid

  return $ countContainedTiles (Set.map fst loop) grid
  where
    countContainedTiles visited grid =
      Grid.indexedFoldl
        ( \y count ->
            fst . Grid.indexedRowFoldl (checkBoundaries visited grid . (y,)) (count, Outside)
        )
        0
        grid

    checkBoundaries visited grid coord acc@(count, containment) item
      | (coord `elem` visited) && causesMeaningfulContainmentChange grid coord item = (count, flipContainment containment)
      | (coord `notElem` visited) && isInside containment = (count + 1, containment)
      | otherwise = acc

    causesMeaningfulContainmentChange grid coord (Pipe Start) =
      case startActsAs grid (coord, Start) of
        Nothing -> False
        Just pipe -> causesMeaningfulContainmentChange grid coord (Pipe pipe)
    causesMeaningfulContainmentChange _ _ item = item `elem` (Pipe <$> [SWBend, SEBend, Vertical])
