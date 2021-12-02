module Day05 exposing (puzzle1, puzzle2)

import Dict
import Parser exposing ((|.), (|=), Parser)



---- PARSING ----


type alias Coords =
    ( Int, Int )


type Line
    = Horizontal ( Coords, Coords )
    | Vertical ( Coords, Coords )
    | Diagonal ( Coords, Coords )


coordsParser : Parser Coords
coordsParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int


lineParser : Parser Line
lineParser =
    Parser.succeed Tuple.pair
        |= coordsParser
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= coordsParser
        |> Parser.map
            (\( ( x1, y1 ), ( x2, y2 ) ) ->
                if x1 == x2 then
                    Vertical
                        ( ( x1, min y1 y2 )
                        , ( x2, max y1 y2 )
                        )

                else if y1 == y2 then
                    Horizontal
                        ( ( min x1 x2, y1 )
                        , ( max x1 x2, y2 )
                        )

                else
                    Diagonal <|
                        if x1 > x2 then
                            ( ( x2, y2 ), ( x1, y1 ) )

                        else
                            ( ( x1, y1 ), ( x2, y2 ) )
            )


parse : String -> List Line
parse input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.filterMap (Parser.run lineParser >> Result.toMaybe)


isDiagonal : Line -> Bool
isDiagonal line =
    case line of
        Diagonal _ ->
            True

        _ ->
            False


coordsInLine : Line -> List Coords
coordsInLine line =
    case line of
        Horizontal ( ( x1, y ), ( x2, _ ) ) ->
            List.range 0 (x2 - x1)
                |> List.map (\offset -> ( x1 + offset, y ))

        Vertical ( ( x, y1 ), ( _, y2 ) ) ->
            List.range 0 (y2 - y1)
                |> List.map (\offset -> ( x, y1 + offset ))

        Diagonal ( ( x1, y1 ), ( x2, y2 ) ) ->
            let
                dx =
                    x2 - x1

                yFactor =
                    clamp -1 1 <| y2 - y1
            in
            List.range 0 dx
                |> List.map (\offset -> ( x1 + offset, y1 + offset * yFactor ))



---- PUZZLES ----


countCoordsWithMultipleOverlaps : List Line -> Int
countCoordsWithMultipleOverlaps lines =
    lines
        |> List.foldl
            (\line acc ->
                line
                    |> coordsInLine
                    |> List.foldl
                        (\coord acc_ ->
                            Dict.update coord
                                (\maybeCount ->
                                    case maybeCount of
                                        Nothing ->
                                            Just 1

                                        Just count ->
                                            Just <| count + 1
                                )
                                acc_
                        )
                        acc
            )
            Dict.empty
        |> Dict.filter (\_ count -> count > 1)
        |> Dict.values
        |> List.length


puzzle1 : String -> Int
puzzle1 input =
    input
        |> parse
        |> List.filter (not << isDiagonal)
        |> countCoordsWithMultipleOverlaps


puzzle2 : String -> Int
puzzle2 input =
    input
        |> parse
        |> countCoordsWithMultipleOverlaps
