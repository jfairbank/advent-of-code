module Day14Tests exposing (suite)

import Day14 exposing (puzzle1, puzzle2)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Day14"
        [ describe "puzzle1"
            [ test "solves example input" <|
                \_ ->
                    puzzle1 exampleInput
                        |> Expect.equal 1588
            , test "solves puzzle input" <|
                \_ ->
                    puzzle1 puzzleInput
                        |> Expect.equal 2587
            ]
        , describe "puzzle2"
            [ test "solves example input" <|
                \_ ->
                    puzzle2 exampleInput
                        |> Expect.equal 2188189693529
            , test "solves puzzle input" <|
                \_ ->
                    puzzle2 puzzleInput
                        |> Expect.equal 3318837563123
            ]
        ]


exampleInput : String
exampleInput =
    """
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"""


puzzleInput : String
puzzleInput =
    """
VHCKBFOVCHHKOHBPNCKO

SO -> F
OP -> V
NF -> F
BO -> V
BH -> S
VB -> B
SV -> B
BK -> S
KC -> N
SP -> O
CP -> O
VN -> O
HO -> S
PC -> B
CS -> O
PO -> K
KF -> B
BP -> K
VO -> O
HB -> N
PH -> O
FF -> O
FB -> K
CC -> H
FK -> F
HV -> P
CO -> S
OC -> N
KV -> V
SS -> O
FC -> O
NP -> B
OH -> B
OF -> K
KB -> K
BN -> C
OK -> C
NC -> O
NO -> O
FS -> C
VP -> K
KP -> S
VS -> B
VV -> N
NN -> P
KH -> P
OB -> H
HP -> H
KK -> H
FH -> F
KS -> V
BS -> V
SN -> H
CB -> B
HN -> K
SB -> O
OS -> K
BC -> H
OV -> N
PN -> B
VH -> N
SK -> C
PV -> K
VC -> N
PF -> S
NB -> B
PP -> S
NS -> F
PB -> B
CV -> C
HK -> P
PK -> S
NH -> B
SH -> V
KO -> H
NV -> B
HH -> V
FO -> O
CK -> O
VK -> F
HF -> O
BF -> C
BV -> P
KN -> K
VF -> C
FN -> V
ON -> C
SF -> F
SC -> C
OO -> S
FP -> K
PS -> C
NK -> O
BB -> V
HC -> H
FV -> V
CH -> N
HS -> V
CF -> F
CN -> S
    """
