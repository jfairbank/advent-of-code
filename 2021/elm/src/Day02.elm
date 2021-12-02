module Day02 exposing (puzzle1, puzzle2)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , int
        , keyword
        , oneOf
        , spaces
        , succeed
        )



---- DIRECTIONS ----


type Direction
    = Forward Int
    | Down Int
    | Up Int


directionParser : Parser Direction
directionParser =
    oneOf
        [ succeed Forward
            |. keyword "forward"
            |. spaces
            |= int
        , succeed Down
            |. keyword "down"
            |. spaces
            |= int
        , succeed Up
            |. keyword "up"
            |. spaces
            |= int
        ]


parseDirection : String -> Maybe Direction
parseDirection input =
    input
        |> Parser.run directionParser
        |> Result.toMaybe


parseDirections : String -> List Direction
parseDirections input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.filterMap parseDirection



---- COORDS ----


type Horizontal
    = Horizontal Int


type Depth
    = Depth Int


type Aim
    = Aim Int


type alias Coords =
    ( Horizontal, Depth )


type alias CoordsWithAim =
    ( Horizontal, Depth, Aim )


multiplyCoords : Coords -> Int
multiplyCoords ( Horizontal h, Depth d ) =
    h * d


coordsWithAimToCoords : CoordsWithAim -> Coords
coordsWithAimToCoords ( horizontal, depth, _ ) =
    ( horizontal, depth )



---- PUZZLES ----


puzzle1 : String -> Int
puzzle1 input =
    let
        applyDirection direction ( (Horizontal h) as horizontal, (Depth d) as depth ) =
            case direction of
                Forward amount ->
                    ( Horizontal <| h + amount
                    , depth
                    )

                Up amount ->
                    ( horizontal
                    , Depth <| d - amount
                    )

                Down amount ->
                    ( horizontal
                    , Depth <| d + amount
                    )
    in
    input
        |> parseDirections
        |> List.foldl applyDirection ( Horizontal 0, Depth 0 )
        |> multiplyCoords


puzzle2 : String -> Int
puzzle2 input =
    let
        applyDirection direction ( (Horizontal h) as horizontal, (Depth d) as depth, (Aim a) as aim ) =
            case direction of
                Forward amount ->
                    ( Horizontal <| h + amount
                    , Depth <| d + a * amount
                    , aim
                    )

                Up amount ->
                    ( horizontal
                    , depth
                    , Aim <| a - amount
                    )

                Down amount ->
                    ( horizontal
                    , depth
                    , Aim <| a + amount
                    )
    in
    input
        |> parseDirections
        |> List.foldl applyDirection ( Horizontal 0, Depth 0, Aim 0 )
        |> coordsWithAimToCoords
        |> multiplyCoords
