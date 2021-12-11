module Day10 exposing (puzzle1, puzzle2)

import Array exposing (Array)
import Dict exposing (Dict)
import Stack exposing (Stack)


type Symbol
    = Paren
    | SquareBracket
    | CurlyBracket
    | AngleBracket


type TaggedSymbol
    = Opening OpeningSymbol
    | Closing ClosingSymbol


type OpeningSymbol
    = OpeningSymbol Symbol


type ClosingSymbol
    = ClosingSymbol Symbol


type Line
    = Corrupted ClosingSymbol
    | Incomplete (List ClosingSymbol)
    | Complete


type alias RawLine =
    List TaggedSymbol


parseChar : Char -> Maybe TaggedSymbol
parseChar =
    let
        taggedSymbolMappings : Dict Char TaggedSymbol
        taggedSymbolMappings =
            Dict.fromList
                [ ( '(', Opening <| OpeningSymbol Paren )
                , ( ')', Closing <| ClosingSymbol Paren )
                , ( '[', Opening <| OpeningSymbol SquareBracket )
                , ( ']', Closing <| ClosingSymbol SquareBracket )
                , ( '{', Opening <| OpeningSymbol CurlyBracket )
                , ( '}', Closing <| ClosingSymbol CurlyBracket )
                , ( '<', Opening <| OpeningSymbol AngleBracket )
                , ( '>', Closing <| ClosingSymbol AngleBracket )
                ]
    in
    \char -> Dict.get char taggedSymbolMappings


parse : String -> List RawLine
parse =
    String.trim
        >> String.split "\n"
        >> List.map (String.toList >> List.filterMap parseChar)


openingToClosing : OpeningSymbol -> ClosingSymbol
openingToClosing (OpeningSymbol symbol) =
    ClosingSymbol symbol


closes : OpeningSymbol -> ClosingSymbol -> Bool
closes (OpeningSymbol openingSymbol) (ClosingSymbol closingSymbol) =
    openingSymbol == closingSymbol


processLine : RawLine -> Line
processLine =
    let
        helper : Stack OpeningSymbol -> RawLine -> Line
        helper stack line =
            case line of
                [] ->
                    if Stack.isEmpty stack then
                        Complete

                    else
                        stack
                            |> Stack.toList
                            |> List.map openingToClosing
                            |> Incomplete

                (Opening openingSymbol) :: remainingSymbols ->
                    helper (Stack.insert openingSymbol stack) remainingSymbols

                (Closing closingSymbol) :: remainingSymbols ->
                    case Stack.pop stack of
                        Just ( openingSymbol, remainingStack ) ->
                            if closes openingSymbol closingSymbol then
                                helper remainingStack remainingSymbols

                            else
                                Corrupted closingSymbol

                        Nothing ->
                            Corrupted closingSymbol
    in
    helper Stack.empty


middle : Array a -> Maybe a
middle array =
    let
        floatLength =
            toFloat <| Array.length array

        index =
            floor (floatLength / 2)
    in
    Array.get index array



---- PUZZLES ----


puzzle1 : String -> Int
puzzle1 input =
    let
        tally : ClosingSymbol -> Int
        tally (ClosingSymbol symbol) =
            case symbol of
                Paren ->
                    3

                SquareBracket ->
                    57

                CurlyBracket ->
                    1197

                AngleBracket ->
                    25137
    in
    input
        |> parse
        |> List.filterMap
            (\line ->
                case processLine line of
                    Corrupted closingChar ->
                        Just <| tally closingChar

                    _ ->
                        Nothing
            )
        |> List.sum


puzzle2 : String -> Maybe Int
puzzle2 input =
    let
        tally : ClosingSymbol -> Int
        tally (ClosingSymbol symbol) =
            case symbol of
                Paren ->
                    1

                SquareBracket ->
                    2

                CurlyBracket ->
                    3

                AngleBracket ->
                    4
    in
    input
        |> parse
        |> List.filterMap
            (\line ->
                case processLine line of
                    Incomplete closingSymbols ->
                        Just <| List.foldl (\symbol score -> score * 5 + tally symbol) 0 closingSymbols

                    _ ->
                        Nothing
            )
        |> List.sort
        |> Array.fromList
        |> middle
