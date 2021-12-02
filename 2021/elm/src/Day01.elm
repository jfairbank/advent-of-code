module Day01 exposing
    ( puzzle1
    , puzzle1Golf
    , puzzle1Recursive
    , puzzle2
    , puzzle2Golf
    , puzzle2Recursive
    )

import Array
import List.Extra



---- HELPERS ----


infinity : Float
infinity =
    1 / 0



---- PUZZLES ----


puzzle1 : List Float -> Float
puzzle1 depths =
    depths
        |> List.foldl
            (\depth ( prev, count ) ->
                ( depth
                , count + clamp 0 1 (depth - prev)
                )
            )
            ( infinity, 0 )
        |> Tuple.second


puzzle1Golf : List Float -> Float
puzzle1Golf =
    List.Extra.groupsOfWithStep 2 1
        >> List.foldl (List.foldl (-) 0 >> clamp 0 1 >> (+)) 0


puzzle1Recursive : List Float -> Float
puzzle1Recursive depths =
    let
        helper depths_ prev acc =
            case depths_ of
                [] ->
                    acc

                depth :: remaining ->
                    helper remaining depth (acc + clamp 0 1 (depth - prev))
    in
    helper depths infinity 0


puzzle2 : List Float -> Float
puzzle2 depths =
    let
        arrayDepths =
            Array.fromList depths

        get index =
            Array.get index arrayDepths
    in
    arrayDepths
        |> Array.indexedMap
            (\index depth ->
                Maybe.map2
                    (\prev2 prev1 -> prev2 + prev1 + depth)
                    (get <| index - 2)
                    (get <| index - 1)
            )
        |> Array.toList
        |> List.filterMap identity
        |> puzzle1


puzzle2Golf : List Float -> Float
puzzle2Golf =
    List.Extra.groupsOfWithStep 3 1 >> List.map List.sum >> puzzle1


type WithPrevious a
    = None
    | One a
    | Two a a


puzzle2Recursive : List Float -> Float
puzzle2Recursive depths =
    let
        helper depths_ withPrevious acc =
            case depths_ of
                [] ->
                    acc

                depth :: remaining ->
                    let
                        ( newWithPrevious, newAcc ) =
                            case withPrevious of
                                None ->
                                    ( One depth, acc )

                                One prev ->
                                    ( Two prev depth, acc )

                                Two prev2 prev1 ->
                                    ( Two prev1 depth, prev2 + prev1 + depth :: acc )
                    in
                    helper remaining newWithPrevious newAcc
    in
    helper depths None []
        |> List.reverse
        |> puzzle1
