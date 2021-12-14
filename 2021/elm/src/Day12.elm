module Day12 exposing (puzzle1, puzzle2)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Utils.Parser



---- CAVE ----


type alias CaveName =
    String


type Big
    = Big CaveName


type Small
    = Small CaveName


type Cave
    = StartCave
    | EndCave
    | BigCave Big
    | SmallCave Small


type Connection
    = Connection Cave Cave


keyStart : CaveName
keyStart =
    "start"


keyEnd : CaveName
keyEnd =
    "end"


caveName : Cave -> CaveName
caveName cave =
    case cave of
        StartCave ->
            keyStart

        EndCave ->
            keyEnd

        BigCave (Big name) ->
            name

        SmallCave (Small name) ->
            name



---- GRAPH ----


type alias Graph =
    Dict CaveName (List Cave)


getConnections : Cave -> Graph -> List Cave
getConnections cave graph =
    graph
        |> Dict.get (caveName cave)
        |> Maybe.withDefault []



---- PARSER ----


caveParser : Parser Cave
caveParser =
    Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString
        |> Parser.map
            (\name ->
                if name == keyStart then
                    StartCave

                else if name == keyEnd then
                    EndCave

                else if String.toUpper name == name then
                    BigCave <| Big name

                else
                    SmallCave <| Small name
            )


connectionParser : Parser Connection
connectionParser =
    Parser.succeed Connection
        |= caveParser
        |. Parser.symbol "-"
        |= caveParser
        |. Utils.Parser.newlineOrEnd


connectionsParser : Parser (List Connection)
connectionsParser =
    Parser.loop [] <|
        \connections ->
            Parser.oneOf
                [ Parser.end
                    |> Parser.map (\_ -> Parser.Done <| List.reverse connections)
                , Parser.succeed (\connection -> Parser.Loop <| connection :: connections)
                    |= connectionParser
                ]


parser : Parser Graph
parser =
    Parser.map
        (List.foldl
            (\(Connection cave1 cave2) acc ->
                [ ( cave1, cave2 ), ( cave2, cave1 ) ]
                    |> List.foldl
                        (\( from, to ) acc_ ->
                            let
                                newConnections =
                                    [ to ]
                            in
                            Dict.update (caveName from)
                                (Maybe.map (Just << List.append newConnections)
                                    >> Maybe.withDefault (Just newConnections)
                                )
                                acc_
                        )
                        acc
            )
            Dict.empty
        )
        connectionsParser


parse : String -> Graph
parse =
    String.trim
        >> Parser.run parser
        >> Result.toMaybe
        >> Maybe.withDefault Dict.empty



---- VISITOR ----


type alias BaseVisitor a =
    { a | visited : Set CaveName }


baseVisit : Cave -> BaseVisitor a -> BaseVisitor a
baseVisit cave visitor =
    { visitor | visited = Set.insert (caveName cave) visitor.visited }


baseHasBeenVisited : Cave -> BaseVisitor a -> Bool
baseHasBeenVisited cave { visited } =
    Set.member (caveName cave) visited


type Visitor state
    = Visitor (BaseVisitor { canVisit : Cave -> Visitor state -> Bool })
    | VisitorWithState
        (BaseVisitor
            { state : state
            , canVisit : Cave -> Visitor state -> state -> Bool
            , updateState : Cave -> Visitor state -> state -> state
            }
        )


newVisitor : { canVisit : Cave -> Visitor state -> Bool } -> Visitor state
newVisitor config =
    Visitor
        { visited = Set.empty
        , canVisit = config.canVisit
        }


newVisitorWithState :
    { state : state
    , canVisit : Cave -> Visitor state -> state -> Bool
    , updateState : Cave -> Visitor state -> state -> state
    }
    -> Visitor state
newVisitorWithState config =
    VisitorWithState
        { visited = Set.empty
        , state = config.state
        , canVisit = config.canVisit
        , updateState = config.updateState
        }


canVisit : Cave -> Visitor state -> Bool
canVisit cave visitor =
    case visitor of
        Visitor v ->
            v.canVisit cave visitor

        VisitorWithState v ->
            v.canVisit cave visitor v.state


visit : Cave -> Visitor state -> Visitor state
visit cave visitor =
    case visitor of
        Visitor v ->
            Visitor <| baseVisit cave v

        VisitorWithState v ->
            VisitorWithState <| baseVisit cave { v | state = v.updateState cave visitor v.state }


hasBeenVisited : Cave -> Visitor state -> Bool
hasBeenVisited cave visitor =
    case visitor of
        Visitor v ->
            baseHasBeenVisited cave v

        VisitorWithState v ->
            baseHasBeenVisited cave v



---- PUZZLES ----


puzzle : Visitor state -> String -> Maybe Int
puzzle initialVisitor input =
    let
        graph : Graph
        graph =
            parse input

        traverse : Visitor state -> Cave -> Maybe (List (List Cave))
        traverse visitor cave =
            case cave of
                EndCave ->
                    Just [ [ cave ] ]

                _ ->
                    let
                        updatedVisitor : Visitor state
                        updatedVisitor =
                            visit cave visitor

                        connections : List Cave
                        connections =
                            graph
                                |> getConnections cave
                                |> List.filter (\connectedCave -> canVisit connectedCave updatedVisitor)
                    in
                    if List.isEmpty connections then
                        Nothing

                    else
                        connections
                            |> List.filterMap
                                (traverse updatedVisitor
                                    >> Maybe.map (List.map ((::) cave))
                                )
                            |> List.concat
                            |> Just
    in
    StartCave
        |> traverse initialVisitor
        |> Maybe.map List.length


puzzle1 : String -> Maybe Int
puzzle1 =
    puzzle <|
        newVisitor
            { canVisit =
                \cave visitor ->
                    case cave of
                        BigCave _ ->
                            True

                        _ ->
                            not <| hasBeenVisited cave visitor
            }


puzzle2 : String -> Maybe Int
puzzle2 =
    puzzle <|
        newVisitorWithState
            { state = Nothing
            , canVisit =
                \cave visitor revisitedSmallCave ->
                    case cave of
                        BigCave _ ->
                            True

                        SmallCave _ ->
                            (not <| hasBeenVisited cave visitor) || revisitedSmallCave == Nothing

                        _ ->
                            not <| hasBeenVisited cave visitor
            , updateState =
                \cave visitor revisitedSmallCave ->
                    case ( revisitedSmallCave, cave, hasBeenVisited cave visitor ) of
                        ( Nothing, SmallCave smallCave, True ) ->
                            Just smallCave

                        _ ->
                            revisitedSmallCave
            }
