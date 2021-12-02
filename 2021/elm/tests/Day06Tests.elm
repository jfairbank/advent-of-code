module Day06Tests exposing (suite)

import Day06 exposing (puzzle1, puzzle2)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day06"
        [ describe "puzzle1"
            [ test "solves example input" <|
                \_ ->
                    puzzle1 exampleInput
                        |> Expect.equal 5934
            , test "solves puzzle input" <|
                \_ ->
                    puzzle1 puzzleInput
                        |> Expect.equal 352872
            ]
        , describe "puzzle2"
            [ test "solves example input" <|
                \_ ->
                    puzzle2 exampleInput
                        |> Expect.equal 26984457539
            , test "solves puzzle input" <|
                \_ ->
                    puzzle2 puzzleInput
                        |> Expect.equal 1604361182149
            ]
        ]


exampleInput : String
exampleInput =
    "3,4,3,1,2"


puzzleInput : String
puzzleInput =
    """
1,2,4,5,5,5,2,1,3,1,4,3,2,1,5,5,1,2,3,4,4,1,2,3,2,1,4,4,1,5,5,1,3,4,4,4,1,2,2,5,1,5,5,3,2,3,1,1,3,5,1,1,2,4,2,3,1,1,2,1,3,1,2,1,1,2,1,2,2,1,1,1,1,5,4,5,2,1,3,2,4,1,1,3,4,1,4,1,5,1,4,1,5,3,2,3,2,2,4,4,3,3,4,3,4,4,3,4,5,1,2,5,2,1,5,5,1,3,4,2,2,4,2,2,1,3,2,5,5,1,3,3,4,3,5,3,5,5,4,5,1,1,4,1,4,5,1,1,1,4,1,1,4,2,1,4,1,3,4,4,3,1,2,2,4,3,3,2,2,2,3,5,5,2,3,1,5,1,1,1,1,3,1,4,1,4,1,2,5,3,2,4,4,1,3,1,1,1,3,4,4,1,1,2,1,4,3,4,2,2,3,2,4,3,1,5,1,3,1,4,5,5,3,5,1,3,5,5,4,2,3,2,4,1,3,2,2,2,1,3,4,2,5,2,5,3,5,5,1,1,1,2,2,3,1,4,4,4,5,4,5,5,1,4,5,5,4,1,1,5,3,3,1,4,1,3,1,1,4,1,5,2,3,2,3,1,2,2,2,1,1,5,1,4,5,2,4,2,2,3
    """
