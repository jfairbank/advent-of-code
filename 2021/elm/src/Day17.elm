module Day17 exposing (puzzle1, puzzle1ByFormula, puzzle2)

import Parser exposing ((|.), (|=), Parser)



---- HELPERS ----


signum : Int -> Int
signum =
    clamp -1 1



---- RANGES ----


type Range
    = Range Int Int


type RangeOutside
    = LessThan
    | GreaterThan


type RangeComparison
    = Inside
    | Outside RangeOutside


createRange : Int -> Int -> Result String Range
createRange start end =
    if start < end then
        Ok <| Range start end

    else if start > end then
        Err "Range must be increasing"

    else
        Err "Range must have at least two different"


rangeStart : Range -> Int
rangeStart (Range n _) =
    n


rangeEnd : Range -> Int
rangeEnd (Range _ n) =
    n


compareToRange : Int -> Range -> RangeComparison
compareToRange n (Range rangeMin rangeMax) =
    if n < rangeMin then
        Outside LessThan

    else if n > rangeMax then
        Outside GreaterThan

    else
        Inside


type XRange
    = XRange Range


type YRange
    = YRange Range


type alias Ranges =
    ( XRange, YRange )


rangeNumberParser : Parser Int
rangeNumberParser =
    let
        baseParser : Parser Int
        baseParser =
            Parser.chompWhile Char.isDigit
                |> Parser.mapChompedString (\value () -> String.toInt value)
                |> Parser.andThen
                    (\maybeInt ->
                        case maybeInt of
                            Nothing ->
                                Parser.problem "Expected int"

                            Just int ->
                                Parser.succeed int
                    )
    in
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= baseParser
        , baseParser
        ]


rangeParser : String -> Parser Range
rangeParser coordinate =
    Parser.succeed createRange
        |. Parser.keyword coordinate
        |. Parser.symbol "="
        |= rangeNumberParser
        |. Parser.symbol ".."
        |= rangeNumberParser
        |> Parser.andThen
            (\result ->
                case result of
                    Ok range ->
                        Parser.succeed range

                    Err error ->
                        Parser.problem error
            )


rangesParser : Parser Ranges
rangesParser =
    Parser.succeed Tuple.pair
        |= rangeParser "x"
        |. Parser.symbol ","
        |. Parser.spaces
        |= rangeParser "y"
        |> Parser.map (Tuple.mapBoth XRange YRange)


parser : Parser Ranges
parser =
    Parser.keyword "target"
        |. Parser.spaces
        |. Parser.keyword "area"
        |. Parser.symbol ":"
        |. Parser.spaces
        |> Parser.andThen (always rangesParser)


parse : String -> Maybe Ranges
parse input =
    input
        |> String.trim
        |> Parser.run parser
        |> Result.toMaybe



---- VECTOR ----


type alias Vector a b =
    ( a, b )



---- POSITIONS ----


type XPosition
    = XPosition Int


type YPosition
    = YPosition Int


yPositionToInt : YPosition -> Int
yPositionToInt (YPosition y) =
    y


originYPosition : YPosition
originYPosition =
    YPosition 0


type alias Coords =
    Vector XPosition YPosition


origin : Coords
origin =
    ( XPosition 0, originYPosition )



---- VELOCITIES ----


type XVelocity
    = XVelocity Int


type YVelocity
    = YVelocity Int


type alias Velocities =
    Vector XVelocity YVelocity



---- PUZZLES ----


landsAt : Coords -> YPosition -> Velocities -> Ranges -> Maybe YPosition
landsAt ( XPosition x, YPosition y ) ((YPosition maxY) as maxYPosition) ( XVelocity xv, YVelocity yv ) (( XRange xRange, YRange yRange ) as ranges) =
    case ( compareToRange x xRange, compareToRange y yRange ) of
        ( Inside, Inside ) ->
            Just maxYPosition

        ( Outside GreaterThan, _ ) ->
            Nothing

        ( _, Outside LessThan ) ->
            Nothing

        _ ->
            let
                updatedXPosition : XPosition
                updatedXPosition =
                    XPosition <| x + xv

                updatedYPosition : YPosition
                updatedYPosition =
                    YPosition <| y + yv

                updatedXvelocity : XVelocity
                updatedXvelocity =
                    XVelocity <| xv - signum xv

                updatedYvelocity : YVelocity
                updatedYvelocity =
                    YVelocity <| yv - 1
            in
            landsAt
                ( updatedXPosition, updatedYPosition )
                (YPosition <| max maxY <| yPositionToInt updatedYPosition)
                ( updatedXvelocity, updatedYvelocity )
                ranges


solve : Ranges -> List YPosition
solve (( XRange xRange, YRange yRange ) as ranges) =
    let
        xCandidates : List Int
        xCandidates =
            List.range 1 <| rangeEnd xRange

        yCandidates : List Int
        yCandidates =
            List.range (rangeStart yRange) (negate <| rangeStart yRange)
    in
    List.concatMap
        (\xv ->
            List.filterMap
                (\yv ->
                    let
                        initialVelocities : Velocities
                        initialVelocities =
                            ( XVelocity xv, YVelocity yv )
                    in
                    landsAt origin originYPosition initialVelocities ranges
                )
                yCandidates
        )
        xCandidates


puzzle1 : String -> Maybe Int
puzzle1 input =
    input
        |> parse
        |> Maybe.andThen
            (solve
                >> List.map yPositionToInt
                >> List.maximum
            )


puzzle1ByFormula : String -> Maybe Int
puzzle1ByFormula input =
    input
        |> parse
        |> Maybe.map
            (\( _, YRange yRange ) ->
                let
                    start : Float
                    start =
                        yRange
                            |> rangeStart
                            |> abs
                            |> toFloat
                in
                floor (start * ((start - 1) / 2))
            )


puzzle2 : String -> Maybe Int
puzzle2 input =
    input
        |> parse
        |> Maybe.map (solve >> List.length)
