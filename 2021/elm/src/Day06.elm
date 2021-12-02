module Day06 exposing (puzzle1, puzzle2)

import Array
import Array.Extra
import NonNegativeInt exposing (NonNegativeInt)



---- FISH ----


type Fish
    = Fish NonNegativeInt


parse : String -> List Fish
parse input =
    input
        |> String.trim
        |> String.split ","
        |> List.filterMap (String.toInt >> Maybe.andThen NonNegativeInt.fromInt >> Maybe.map Fish)



---- PUZZLES ----


puzzle : Int -> String -> Int
puzzle numDays input =
    let
        initialCounts =
            input
                |> parse
                |> List.foldl
                    (\(Fish timer) acc ->
                        let
                            index =
                                NonNegativeInt.toInt timer
                        in
                        Array.Extra.update index ((+) 1) acc
                    )
                    (Array.repeat 9 0)
    in
    List.range 0 (numDays - 1)
        |> List.foldl
            (\_ acc ->
                let
                    ( maybeNumFishToProduceAChild, otherCounts ) =
                        acc
                            |> Array.Extra.splitAt 1
                            |> Tuple.mapFirst (Array.get 0)
                in
                maybeNumFishToProduceAChild
                    |> Maybe.map
                        (\numFishToProduceAChild ->
                            otherCounts
                                |> Array.Extra.update 6 ((+) numFishToProduceAChild)
                                |> Array.push numFishToProduceAChild
                        )
                    |> Maybe.withDefault acc
            )
            initialCounts
        |> Array.toList
        |> List.sum


puzzle1 : String -> Int
puzzle1 =
    puzzle 80


puzzle2 : String -> Int
puzzle2 =
    puzzle 256
