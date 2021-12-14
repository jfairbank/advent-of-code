module Day13 exposing (puzzle1, puzzle2)

import Array
import NaiveMatrix as Matrix exposing (Coords)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Set.Extra
import Utils.Parser
import Utils.Tuple2



---- INSTRUCTIONS ----


type Line
    = Line Int


type Axis
    = HorizontalAxis Line
    | VerticalAxis Line


type Instruction
    = Fold Axis



---- PAPER ----


type alias Paper =
    { coordinates : Set Coords
    , instructions : List Instruction
    }


newPaper : Paper
newPaper =
    Paper Set.empty []


type InstructionResult
    = Folded Paper
    | Unchanged


applyNextInstruction : Paper -> InstructionResult
applyNextInstruction paper =
    case paper.instructions of
        (Fold axis) :: remaining ->
            let
                foldCoordinate : Line -> Int -> Maybe Int
                foldCoordinate (Line line) coord =
                    if coord == line then
                        Nothing

                    else if coord < line then
                        Just coord

                    else
                        Just (line * 2 - coord)

                updatedCoordinates : Set Coords
                updatedCoordinates =
                    case axis of
                        HorizontalAxis line ->
                            Set.Extra.filterMap
                                (Tuple.mapBoth Just (foldCoordinate line) >> Utils.Tuple2.sequenceMaybe)
                                paper.coordinates

                        VerticalAxis line ->
                            Set.Extra.filterMap
                                (Tuple.mapBoth (foldCoordinate line) Just >> Utils.Tuple2.sequenceMaybe)
                                paper.coordinates
            in
            Folded
                { paper
                    | coordinates = updatedCoordinates
                    , instructions = remaining
                }

        [] ->
            Unchanged


applyInstructionsTimes : Int -> Paper -> Paper
applyInstructionsTimes step paper =
    if step <= 0 then
        paper

    else
        case applyNextInstruction paper of
            Folded updatedPaper ->
                applyInstructionsTimes (step - 1) updatedPaper

            Unchanged ->
                paper


applyAllInstructions : Paper -> Paper
applyAllInstructions paper =
    case applyNextInstruction paper of
        Folded updatedPaper ->
            applyAllInstructions updatedPaper

        Unchanged ->
            paper


countVisibleDots : Paper -> Int
countVisibleDots paper =
    Set.size paper.coordinates


paperSize : Paper -> ( Int, Int )
paperSize paper =
    Set.foldl
        (\( x, y ) ->
            Tuple.mapBoth
                (max (x + 1))
                (max (y + 1))
        )
        ( 0, 0 )
        paper.coordinates


printPaper : Paper -> String
printPaper paper =
    paper.coordinates
        |> Set.foldl
            (\coords acc -> Matrix.set coords '#' acc)
            (Matrix.new (paperSize paper) '.')
        |> Matrix.foldlRows
            (\_ row acc -> String.fromList (Array.toList row) :: acc)
            []
        |> List.reverse
        |> String.join "\n"



---- PARSERS ----


type ParserStep
    = StepCoords
    | StepInstructions


coordsParser : Parser Coords
coordsParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int


instructionParser : Parser Instruction
instructionParser =
    Parser.succeed Fold
        |. Parser.keyword "fold"
        |. Utils.Parser.justSpaces
        |. Parser.keyword "along"
        |. Utils.Parser.justSpaces
        |= Parser.oneOf
            [ Parser.succeed (HorizontalAxis << Line)
                |. Parser.keyword "y"
                |. Parser.symbol "="
                |= Parser.int
            , Parser.succeed (VerticalAxis << Line)
                |. Parser.keyword "x"
                |. Parser.symbol "="
                |= Parser.int
            ]


paperParser : Parser Paper
paperParser =
    Parser.loop ( StepCoords, newPaper ) <|
        \( parserStep, paper ) ->
            case parserStep of
                StepCoords ->
                    Parser.oneOf
                        [ Parser.succeed
                            (\coords ->
                                Parser.Loop
                                    ( parserStep
                                    , { paper | coordinates = Set.insert coords paper.coordinates }
                                    )
                            )
                            |= coordsParser
                            |. Parser.spaces
                        , Parser.succeed ()
                            |> Parser.map (\_ -> Parser.Loop ( StepInstructions, paper ))
                        ]

                StepInstructions ->
                    Parser.oneOf
                        [ Parser.succeed
                            (\instruction ->
                                Parser.Loop
                                    ( parserStep
                                    , { paper | instructions = instruction :: paper.instructions }
                                    )
                            )
                            |= instructionParser
                            |. Parser.spaces
                        , Parser.end
                            |> Parser.map (\_ -> Parser.Done { paper | instructions = List.reverse paper.instructions })
                        ]


parse : String -> Paper
parse =
    String.trim
        >> Parser.run paperParser
        >> Result.toMaybe
        >> Maybe.withDefault newPaper



---- PUZZLES ----


puzzle1 : String -> Int
puzzle1 input =
    input
        |> parse
        |> applyInstructionsTimes 1
        |> countVisibleDots


puzzle2 : String -> Int
puzzle2 input =
    let
        paper : Paper
        paper =
            input
                |> parse
                |> applyAllInstructions

        _ =
            paper
                |> printPaper
                |> Debug.log ""
    in
    countVisibleDots paper
