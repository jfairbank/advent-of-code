module Day12Tests exposing (suite)

import Day12 exposing (puzzle1, puzzle2)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day12"
        [ describe "puzzle1"
            [ test "solves example input 1" <|
                \_ ->
                    puzzle1 exampleInput1
                        |> Expect.equal (Just 10)
            , test "solves example input 2" <|
                \_ ->
                    puzzle1 exampleInput2
                        |> Expect.equal (Just 19)
            , test "solves example input 3" <|
                \_ ->
                    puzzle1 exampleInput3
                        |> Expect.equal (Just 226)
            , test "solves puzzle input" <|
                \_ ->
                    puzzle1 puzzleInput
                        |> Expect.equal (Just 5076)
            ]
        , describe "puzzle2"
            [ test "solves example input 1" <|
                \_ ->
                    puzzle2 exampleInput1
                        |> Expect.equal (Just 36)
            , test "solves example input 2" <|
                \_ ->
                    puzzle2 exampleInput2
                        |> Expect.equal (Just 103)
            , test "solves example input 3" <|
                \_ ->
                    puzzle2 exampleInput3
                        |> Expect.equal (Just 3509)
            , test "solves puzzle input" <|
                \_ ->
                    puzzle2 puzzleInput
                        |> Expect.equal (Just 145643)
            ]
        ]


exampleInput1 : String
exampleInput1 =
    """
start-A
start-b
A-c
A-b
b-d
A-end
b-end
"""


exampleInput2 : String
exampleInput2 =
    """
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
"""


exampleInput3 : String
exampleInput3 =
    """
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
"""


puzzleInput : String
puzzleInput =
    """
end-MY
MY-xc
ho-NF
start-ho
NF-xc
NF-yf
end-yf
xc-TP
MY-qo
yf-TP
dc-NF
dc-xc
start-dc
yf-MY
MY-ho
EM-uh
xc-yf
ho-dc
uh-NF
yf-ho
end-uh
start-NF
    """
