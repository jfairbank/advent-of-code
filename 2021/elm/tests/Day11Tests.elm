module Day11Tests exposing (suite)

import Day11 exposing (puzzle1, puzzle2)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day11"
        [ describe "puzzle1"
            [ test "solves example input" <|
                \_ ->
                    puzzle1 exampleInput
                        |> Expect.equal 1656
            , test "solves puzzle input" <|
                \_ ->
                    puzzle1 puzzleInput
                        |> Expect.equal 1601
            ]
        , describe "puzzle2"
            [ test "solves example input" <|
                \_ ->
                    puzzle2 exampleInput
                        |> Expect.equal 195
            , test "solves puzzle input" <|
                \_ ->
                    puzzle2 puzzleInput
                        |> Expect.equal 368
            ]
        ]


exampleInput : String
exampleInput =
    """
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"""


puzzleInput : String
puzzleInput =
    """
8548335644
6576521782
1223677762
1284713113
6125654778
6435726842
5664175556
1445736556
2248473568
6451473526
    """
