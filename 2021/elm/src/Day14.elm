module Day14 exposing (puzzle1, puzzle2)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Utils.Parser
import Utils.Tuple2



---- MISC HELPERS ----


type alias CountDict comparable =
    Dict comparable Int


changeCountBy : Int -> comparable -> CountDict comparable -> CountDict comparable
changeCountBy amount key dict =
    Dict.update key
        (Maybe.withDefault 0
            >> (+) amount
            >> max 0
            >> Just
        )
        dict


stringToChar : String -> Maybe Char
stringToChar =
    String.uncons >> Maybe.map Tuple.first



---- TYPES ----


type alias Element =
    Char


type alias Pair =
    ( Char, Char )


type alias Template =
    String


type alias Rules =
    Dict Pair Element


type Rule
    = Rule Pair Element


type alias PairCounts =
    CountDict Pair


type alias ElementCounts =
    CountDict Element



---- PARSING ----


type ParserStep
    = StepTemplate
    | StepRules Template Rules


elementParser : Parser Element
elementParser =
    Parser.chompIf Char.isAlpha
        |> Parser.getChompedString
        |> Parser.map (stringToChar >> Maybe.withDefault ' ')


pairParser : Parser Pair
pairParser =
    Parser.succeed Tuple.pair
        |= elementParser
        |= elementParser


ruleParser : Parser Rule
ruleParser =
    Parser.succeed Rule
        |= pairParser
        |. Utils.Parser.justSpaces
        |. Parser.symbol "->"
        |. Utils.Parser.justSpaces
        |= elementParser


templateParser : Parser Template
templateParser =
    Parser.succeed ()
        |. Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString


parser : Parser ( Template, Rules )
parser =
    Parser.loop StepTemplate <|
        \step ->
            case step of
                StepTemplate ->
                    templateParser
                        |. Parser.spaces
                        |> Parser.map (\template -> Parser.Loop <| StepRules template Dict.empty)

                StepRules template rules ->
                    Parser.oneOf
                        [ ruleParser
                            |. Utils.Parser.newlineOrEnd
                            |> Parser.map
                                (\(Rule pair element) ->
                                    Parser.Loop <| StepRules template (Dict.insert pair element rules)
                                )
                        , Parser.succeed ()
                            |> Parser.map (\_ -> Parser.Done ( template, rules ))
                        ]


parse : String -> ( Template, Rules )
parse =
    String.trim
        >> Parser.run parser
        >> Result.toMaybe
        >> Maybe.withDefault ( "", Dict.empty )



---- PUZZLES ----


puzzle : Int -> String -> Int
puzzle numSteps input =
    let
        ( template, rules ) =
            parse input

        initialPairCounts : PairCounts
        initialPairCounts =
            template
                |> String.toList
                |> List.Extra.groupsOfWithStep 2 1
                |> List.foldl
                    (\pairList acc ->
                        pairList
                            |> Utils.Tuple2.fromList
                            |> Maybe.map (\pair -> changeCountBy 1 pair acc)
                            |> Maybe.withDefault acc
                    )
                    Dict.empty

        initialElementCounts : ElementCounts
        initialElementCounts =
            String.foldl (changeCountBy 1) Dict.empty template

        updatedElementCounts : ElementCounts
        updatedElementCounts =
            List.range 1 numSteps
                |> List.foldl
                    (\_ ( pairCountsAcc, elementCountsAcc ) ->
                        pairCountsAcc
                            |> Dict.filter (\_ pairCount -> pairCount > 0)
                            |> Dict.foldl
                                (\(( char1, char2 ) as pair) pairCount (( pairCountsAcc_, elementCountsAcc_ ) as acc_) ->
                                    rules
                                        |> Dict.get pair
                                        |> Maybe.map
                                            (\element ->
                                                ( pairCountsAcc_
                                                    |> changeCountBy pairCount ( char1, element )
                                                    |> changeCountBy pairCount ( element, char2 )
                                                    |> changeCountBy -pairCount pair
                                                , elementCountsAcc_
                                                    |> changeCountBy pairCount element
                                                )
                                            )
                                        |> Maybe.withDefault acc_
                                )
                                ( pairCountsAcc, elementCountsAcc )
                    )
                    ( initialPairCounts, initialElementCounts )
                |> Tuple.second

        ( mostCommonCharCount, leastCommonCharCount ) =
            updatedElementCounts
                |> Dict.foldl
                    (\_ count_ acc ->
                        case acc of
                            Nothing ->
                                Just ( count_, count_ )

                            Just ( mostCommonCount, leastCommonCount ) ->
                                if count_ > mostCommonCount then
                                    Just ( count_, leastCommonCount )

                                else if count_ < leastCommonCount then
                                    Just ( mostCommonCount, count_ )

                                else
                                    acc
                    )
                    Nothing
                |> Maybe.withDefault ( 0, 0 )
    in
    mostCommonCharCount - leastCommonCharCount


puzzle1 : String -> Int
puzzle1 =
    puzzle 10


puzzle2 : String -> Int
puzzle2 =
    puzzle 40
