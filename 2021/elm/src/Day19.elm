module Day19 exposing (puzzle1, puzzle2)

import Array exposing (Array)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Utils.Parser
import Utils.Tuple3


type alias Vector3 =
    ( Int, Int, Int )


manhattanDistance : Vector3 -> Vector3 -> Int
manhattanDistance ( x1, y1, z1 ) ( x2, y2, z2 ) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)


type Beacon
    = Beacon Vector3


type Scanner
    = Scanner Int (List Beacon)


beaconParser : Parser Beacon
beaconParser =
    Parser.succeed Utils.Tuple3.trio
        |= Utils.Parser.signedInt
        |. Parser.symbol ","
        |= Utils.Parser.signedInt
        |. Parser.symbol ","
        |= Utils.Parser.signedInt
        |> Parser.map Beacon


scannerHeaderParser : Parser Int
scannerHeaderParser =
    Parser.succeed identity
        |. Utils.Parser.repeated 3 (Parser.symbol "-")
        |. Utils.Parser.justSpaces
        |. Parser.keyword "scanner"
        |. Utils.Parser.justSpaces
        |= Parser.int
        |. Utils.Parser.justSpaces
        |. Utils.Parser.repeated 3 (Parser.symbol "-")


scannerParser : Parser Scanner
scannerParser =
    Parser.succeed Scanner
        |= scannerHeaderParser
        |. Utils.Parser.newline
        |= Parser.loop []
            (\beacons ->
                Parser.oneOf
                    [ beaconParser
                        |. Parser.spaces
                        |> Parser.map (\beacon -> Parser.Loop <| beacon :: beacons)
                    , Parser.lazy (\_ -> Parser.succeed <| Parser.Done <| List.reverse beacons)
                    ]
            )


parser : Parser (List Scanner)
parser =
    Parser.loop [] <|
        \scanners ->
            Parser.oneOf
                [ scannerParser
                    |> Parser.map (\scanner -> Parser.Loop <| scanner :: scanners)
                , Parser.lazy (\_ -> Parser.succeed <| Parser.Done <| List.reverse scanners)
                ]


parse : String -> Maybe (List Scanner)
parse input =
    input
        |> String.trim
        |> Parser.run parser
        |> Debug.log "result"
        |> Result.toMaybe



---- PUZZLES ----


puzzle1 : String -> Maybe Int
puzzle1 input =
    let
        _ =
            parse input
    in
    Just 0


puzzle2 : String -> Maybe Int
puzzle2 input =
    Just 0
