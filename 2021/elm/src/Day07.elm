module Day07 exposing (puzzle1, puzzle2)

import Array exposing (Array)
import Array.Extra
import Set exposing (Set)



---- PUZZLES ----


puzzle : (Int -> Int) -> String -> Maybe Int
puzzle adjustRate input =
    let
        positions : List Int
        positions =
            input
                |> String.trim
                |> String.split ","
                |> List.filterMap String.toInt

        uniquePositions : Set Int
        uniquePositions =
            Set.fromList positions

        maxPosition : Maybe Int
        maxPosition =
            List.maximum positions

        allPositions : List Int
        allPositions =
            maxPosition
                |> Maybe.map (List.range 0)
                |> Maybe.withDefault []

        positionCounts : Array Int
        positionCounts =
            maxPosition
                |> Maybe.map
                    (\maxPosition_ ->
                        List.foldl
                            (\position acc -> Array.Extra.update position ((+) 1) acc)
                            (Array.repeat (maxPosition_ + 1) 0)
                            positions
                    )
                |> Maybe.withDefault Array.empty
    in
    allPositions
        |> List.map
            (\destination ->
                Set.foldl
                    (\position acc ->
                        let
                            count =
                                positionCounts
                                    |> Array.get position
                                    |> Maybe.withDefault 0

                            constantRate =
                                abs (position - destination)

                            adjustedRate =
                                adjustRate constantRate
                        in
                        acc + adjustedRate * count
                    )
                    0
                    uniquePositions
            )
        |> List.minimum


puzzle1 : String -> Maybe Int
puzzle1 =
    puzzle identity


puzzle2 : String -> Maybe Int
puzzle2 =
    puzzle <|
        \constantRate ->
            let
                floatConstantRate =
                    toFloat constantRate
            in
            floor <| floatConstantRate * (floatConstantRate + 1) / 2
