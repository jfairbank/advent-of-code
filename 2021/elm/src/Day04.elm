module Day04 exposing (puzzle1, puzzle2)

import Array exposing (Array)
import Array.Extra
import List.Extra
import Utils.NaiveMatrix as Matrix exposing (Matrix, Vector)



---- MISC TYPES ----


type alias Selections =
    List Int


type alias Thunk a =
    () -> a


type alias FutureMaybe a =
    Thunk (Maybe a)



---- MATCH ----


type Match
    = Match Int
    | NoMatch Int


isMatch : Match -> Bool
isMatch match =
    case match of
        Match _ ->
            True

        NoMatch _ ->
            False



---- BOARD ----


type alias Board =
    Matrix Match


type Win
    = Win Board
    | NoWin Board


boardFromString : String -> Board
boardFromString input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.map
            (String.split " "
                >> List.filterMap
                    (String.trim
                        >> String.toInt
                        >> Maybe.map NoMatch
                    )
            )
        |> Matrix.fromList


isWinner : Vector Match -> Bool
isWinner =
    Array.Extra.all isMatch


checkBoard : Int -> Board -> Win
checkBoard selection board =
    let
        updatedBoard =
            Matrix.map
                (\match ->
                    case match of
                        NoMatch value ->
                            if value == selection then
                                Match value

                            else
                                match

                        Match _ ->
                            match
                )
                board
    in
    case winningLine updatedBoard of
        Just _ ->
            Win updatedBoard

        Nothing ->
            NoWin updatedBoard


selectFirstWinner : FutureMaybe (Vector Match) -> FutureMaybe (Vector Match) -> Maybe (Vector Match)
selectFirstWinner thunk1 thunk2 =
    let
        maybeVector1 =
            thunk1 ()

        vector1IsWinner =
            maybeVector1
                |> Maybe.map isWinner
                |> Maybe.withDefault False
    in
    if vector1IsWinner then
        maybeVector1

    else
        let
            maybeVector2 =
                thunk2 ()
        in
        maybeVector2
            |> Maybe.andThen
                (\vector2 ->
                    if isWinner vector2 then
                        maybeVector2

                    else
                        Nothing
                )


winningLine : Board -> Maybe (Vector Match)
winningLine board =
    let
        size =
            Matrix.height board

        helper index =
            if index >= size then
                Nothing

            else
                let
                    row =
                        \() -> Matrix.getRow index board

                    column =
                        \() -> Matrix.getColumn index board
                in
                case selectFirstWinner row column of
                    (Just _) as maybeWinner ->
                        maybeWinner

                    Nothing ->
                        helper (index + 1)
    in
    helper 0



---- PUZZLES ----


type Strategy
    = SelectFirst
    | SelectLast


parseInput : String -> Maybe ( Selections, Array Board )
parseInput input =
    input
        |> String.trim
        |> String.split "\n\n"
        |> List.Extra.uncons
        |> Maybe.map
            (\( selections, boards ) ->
                ( selections
                    |> String.split ","
                    |> List.filterMap String.toInt
                , boards
                    |> List.map boardFromString
                    |> Array.fromList
                )
            )


puzzle : Strategy -> String -> Maybe Int
puzzle strategy input =
    let
        helper selections boardIndex boards =
            let
                numBoards =
                    Array.length boards
            in
            case ( selections, boardIndex >= numBoards ) of
                ( [], _ ) ->
                    Nothing

                ( _ :: remainingSelections, True ) ->
                    helper remainingSelections 0 boards

                ( selection :: _, False ) ->
                    let
                        maybeBoard =
                            boards
                                |> Array.get boardIndex
                                |> Maybe.map (checkBoard selection)
                    in
                    case maybeBoard of
                        Just (Win board) ->
                            if strategy == SelectFirst || numBoards == 1 then
                                let
                                    unmarkedSum =
                                        Matrix.foldl
                                            (\_ element acc ->
                                                case element of
                                                    Match _ ->
                                                        acc

                                                    NoMatch value ->
                                                        acc + value
                                            )
                                            0
                                            board
                                in
                                Just <| unmarkedSum * selection

                            else if strategy == SelectLast && numBoards == 0 then
                                Nothing

                            else
                                helper selections boardIndex (Array.Extra.removeAt boardIndex boards)

                        Just (NoWin board) ->
                            helper selections (boardIndex + 1) (Array.set boardIndex board boards)

                        Nothing ->
                            helper selections (boardIndex + 1) boards
    in
    input
        |> parseInput
        |> Maybe.andThen (\( selections, boards ) -> helper selections 0 boards)


puzzle1 : String -> Maybe Int
puzzle1 =
    puzzle SelectFirst


puzzle2 : String -> Maybe Int
puzzle2 =
    puzzle SelectLast
