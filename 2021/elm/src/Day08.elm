module Day08 exposing (puzzle1, puzzle2)

import Dict exposing (Dict)
import Set exposing (Set)
import Set.Extra


type Signal
    = Signal (Set Char)


parseSignal : String -> Signal
parseSignal input =
    input
        |> String.toList
        |> Set.fromList
        |> Signal


type Wire
    = Wire Signal


parseWire : String -> Wire
parseWire =
    Wire << parseSignal


wireToSignal : Wire -> Signal
wireToSignal (Wire signal) =
    signal


type Output
    = Output Signal


parseOutput : String -> Output
parseOutput =
    Output << parseSignal


outputToSignal : Output -> Signal
outputToSignal (Output signal) =
    signal


type Entry
    = Entry (List Wire) (List Output)


type alias Digits =
    { zero : Set Char
    , one : Set Char
    , two : Set Char
    , three : Set Char
    , four : Set Char
    , five : Set Char
    , six : Set Char
    , seven : Set Char
    , eight : Set Char
    , nine : Set Char
    }


emptyDigits : Digits
emptyDigits =
    { zero = Set.empty
    , one = Set.empty
    , two = Set.empty
    , three = Set.empty
    , four = Set.empty
    , five = Set.empty
    , six = Set.empty
    , seven = Set.empty
    , eight = Set.empty
    , nine = Set.empty
    }


parse : String -> List Entry
parse input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.map
            (\entry ->
                let
                    wiresAndOutputValues : List String
                    wiresAndOutputValues =
                        String.split " | " entry

                    wires : List Wire
                    wires =
                        wiresAndOutputValues
                            |> List.head
                            |> Maybe.map (String.split " " >> List.map parseWire)
                            |> Maybe.withDefault []

                    outputValues : List Output
                    outputValues =
                        wiresAndOutputValues
                            |> List.tail
                            |> Maybe.andThen List.head
                            |> Maybe.map (String.split " " >> List.map parseOutput)
                            |> Maybe.withDefault []
                in
                Entry wires outputValues
            )


identifyUniqueDigit : Signal -> Maybe Int
identifyUniqueDigit (Signal chars) =
    case Set.size chars of
        2 ->
            Just 1

        3 ->
            Just 7

        4 ->
            Just 4

        7 ->
            Just 8

        _ ->
            Nothing


identifyDigits : List Signal -> Digits
identifyDigits signals =
    signals
        |> List.map (\signal -> ( signal, identifyUniqueDigit signal ))
        |> List.sortWith
            (\( Signal signal1, digit1 ) ( Signal signal2, digit2 ) ->
                case ( digit1, digit2 ) of
                    ( Just _, Just _ ) ->
                        compare (Set.size signal1) (Set.size signal2)

                    ( Nothing, Just _ ) ->
                        GT

                    ( Just _, Nothing ) ->
                        LT

                    ( Nothing, Nothing ) ->
                        EQ
            )
        |> List.foldl
            (\( Signal chars, digit ) digits ->
                case digit of
                    Just 1 ->
                        { digits | one = chars }

                    Just 7 ->
                        { digits | seven = chars }

                    Just 4 ->
                        { digits | four = chars }

                    Just 8 ->
                        { digits | eight = chars }

                    _ ->
                        let
                            -- Conditions defined in the order they should be checked
                            is9 () =
                                Set.isEmpty digits.nine && Set.Extra.subset digits.four chars

                            is3 () =
                                Set.isEmpty digits.three && Set.Extra.subset digits.seven chars && (Set.size <| Set.diff chars digits.seven) == 2

                            -- This would technically match 9 as well, so 9 is checked first with 4 being a subset of 9
                            is0 () =
                                Set.isEmpty digits.zero && Set.Extra.subset digits.seven chars && (Set.size <| Set.diff chars digits.seven) == 3

                            is6 () =
                                Set.isEmpty digits.six && (Set.size <| Set.diff digits.eight chars) == 1

                            is2 () =
                                Set.isEmpty digits.two && (Set.union digits.four chars == digits.eight)

                            is5 () =
                                Set.isEmpty digits.five
                        in
                        -- Order matters
                        if is9 () then
                            { digits | nine = chars }

                        else if is3 () then
                            { digits | three = chars }

                        else if is0 () then
                            { digits | zero = chars }

                        else if is6 () then
                            { digits | six = chars }

                        else if is2 () then
                            { digits | two = chars }

                        else if is5 () then
                            { digits | five = chars }

                        else
                            digits
            )
            emptyDigits



---- PUZZLES ----


puzzle1 : String -> Int
puzzle1 input =
    input
        |> parse
        |> List.map
            (\(Entry _ outputValues) ->
                outputValues
                    |> List.filterMap (outputToSignal >> identifyUniqueDigit)
                    |> List.length
            )
        |> List.sum


puzzle2 : String -> Int
puzzle2 input =
    input
        |> parse
        |> List.filterMap
            (\(Entry wires outputValues) ->
                let
                    digits : Digits
                    digits =
                        wires
                            |> List.map wireToSignal
                            |> identifyDigits

                    digitsDict : Dict (List Char) Char
                    digitsDict =
                        Dict.fromList
                            [ ( Set.toList digits.zero, '0' )
                            , ( Set.toList digits.one, '1' )
                            , ( Set.toList digits.two, '2' )
                            , ( Set.toList digits.three, '3' )
                            , ( Set.toList digits.four, '4' )
                            , ( Set.toList digits.five, '5' )
                            , ( Set.toList digits.six, '6' )
                            , ( Set.toList digits.seven, '7' )
                            , ( Set.toList digits.eight, '8' )
                            , ( Set.toList digits.nine, '9' )
                            ]
                in
                outputValues
                    |> List.filterMap (\(Output (Signal chars)) -> Dict.get (Set.toList chars) digitsDict)
                    |> String.fromList
                    |> String.toInt
            )
        |> List.sum
