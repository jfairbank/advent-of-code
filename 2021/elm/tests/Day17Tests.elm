module Day17Tests exposing (suite)

import Day17 exposing (puzzle1, puzzle1ByFormula, puzzle2)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day17"
        [ describe "puzzle1"
            [ test "solves example input" <|
                \_ ->
                    puzzle1 exampleInput
                        |> Expect.equal (Just 45)
            , test "solves puzzle input" <|
                \_ ->
                    puzzle1 puzzleInput
                        |> Expect.equal (Just 7503)
            ]
        , describe "puzzle1ByFormula"
            [ test "solves example input" <|
                \_ ->
                    puzzle1ByFormula exampleInput
                        |> Expect.equal (Just 45)
            , test "solves puzzle input" <|
                \_ ->
                    puzzle1ByFormula puzzleInput
                        |> Expect.equal (Just 7503)
            ]
        , describe "puzzle2"
            [ test "solves example input" <|
                \_ ->
                    puzzle2 exampleInput
                        |> Expect.equal (Just 112)
            , test "solves puzzle input" <|
                \_ ->
                    puzzle2 puzzleInput
                        |> Expect.equal (Just 3229)
            ]
        ]


exampleInput : String
exampleInput =
    "target area: x=20..30, y=-10..-5"


puzzleInput : String
puzzleInput =
    "target area: x=124..174, y=-123..-86"
