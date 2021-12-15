module Day03 exposing (puzzle1, puzzle2)

import Array exposing (Array)
import Array.Extra
import Binary
import Bitwise
import Utils.NaiveMatrix as Matrix exposing (Matrix)



---- BIT ----


bitNot : Int -> Int
bitNot value =
    if value == 1 then
        0

    else
        1


{-| Converts 1 -> 1 and 0 -> -1 so they can be easily added to the
counts to determine if there are more 1's or 0's
-}
bitToTally : Int -> Int
bitToTally x =
    Bitwise.shiftLeftBy 1 x - 1


addBitToTally : Int -> Int -> Int
addBitToTally x tally =
    bitToTally x + tally



---- GRID ----


type alias Grid =
    Matrix Int


buildGrid : String -> Grid
buildGrid input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.map (Binary.fromString 1 >> Binary.toIntegers)
        |> Matrix.fromList



---- PUZZLES ----


puzzle1 : String -> Int
puzzle1 input =
    let
        grid =
            buildGrid input

        initCounts =
            Array.repeat (Matrix.width grid) 0

        counts =
            grid
                |> Matrix.foldlRows (\_ -> Array.Extra.map2 addBitToTally) initCounts
                |> Array.map (clamp 0 1)

        gammaRate =
            counts
                |> Array.toList
                |> Binary.fromIntegers

        epsilonRate =
            Binary.not gammaRate
    in
    Binary.toDecimal gammaRate * Binary.toDecimal epsilonRate


determineMostCommonBit : Array Int -> Int
determineMostCommonBit bits =
    let
        count =
            Array.foldl addBitToTally 0 bits
    in
    if count == 0 then
        1

    else
        clamp 0 1 count


determineLeastCommonBit : Array Int -> Int
determineLeastCommonBit =
    determineMostCommonBit >> bitNot


puzzle2 : String -> Maybe Int
puzzle2 input =
    let
        grid =
            buildGrid input

        filterBy j findBit matrix =
            let
                column =
                    Matrix.getColumn j matrix

                bit =
                    Maybe.map findBit column
            in
            if Matrix.height matrix == 1 then
                matrix

            else
                Matrix.filter
                    (Array.get j
                        >> Maybe.map2 (==) bit
                        >> Maybe.withDefault False
                    )
                    matrix

        getFirstAsInt =
            Matrix.getRow 0
                >> Maybe.map
                    (Array.toList
                        >> Binary.fromIntegers
                        >> Binary.toDecimal
                    )

        ( oxygenGeneratorRating, co2ScrubberRating ) =
            grid
                |> Matrix.foldlRows
                    (\j _ ( mostCommonAcc, leastCommonAcc ) ->
                        ( filterBy j determineMostCommonBit mostCommonAcc
                        , filterBy j determineLeastCommonBit leastCommonAcc
                        )
                    )
                    ( grid, grid )
                |> Tuple.mapBoth getFirstAsInt getFirstAsInt
    in
    Maybe.map2 (*) oxygenGeneratorRating co2ScrubberRating
