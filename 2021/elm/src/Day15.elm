module Day15 exposing (puzzle1, puzzle2)

import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)
import Utils.NaiveMatrix as Matrix exposing (Coords, Matrix)



---- MISC HELPERS ----


maxSafeInt : Int
maxSafeInt =
    2 ^ 53 - 1



---- RISKS ----


type Risk
    = Risk Int


riskMap : (Int -> Int) -> Risk -> Risk
riskMap f (Risk risk) =
    Risk <| f risk


riskMap2 : (Int -> Int -> Int) -> Risk -> Risk -> Risk
riskMap2 f (Risk x) (Risk y) =
    Risk <| f x y


addRisks : Risk -> Risk -> Risk
addRisks =
    riskMap2 (+)


riskToInt : Risk -> Int
riskToInt (Risk risk) =
    risk


maxRisk : Risk
maxRisk =
    Risk maxSafeInt


type alias Risks =
    Matrix Risk


type BaseRisks
    = BaseRisks Risks


baseRisksToRisks : BaseRisks -> Risks
baseRisksToRisks (BaseRisks risks) =
    risks


type AccumulatedRisks
    = AccumulatedRisks Risks


accumulatedRisksMap : (Risks -> Risks) -> AccumulatedRisks -> AccumulatedRisks
accumulatedRisksMap f (AccumulatedRisks risks) =
    AccumulatedRisks <| f risks


accumulatedRisksToRisks : AccumulatedRisks -> Risks
accumulatedRisksToRisks (AccumulatedRisks risks) =
    risks



---- PARSER ----


parse : String -> BaseRisks
parse input =
    input
        |> String.trim
        |> String.lines
        |> List.map
            (String.split ""
                >> List.filterMap (String.toInt >> Maybe.map Risk)
            )
        |> Matrix.fromList
        |> BaseRisks



---- QUEUE ----


type alias Queue =
    PriorityQueue ( Coords, Risk )


type alias Visited =
    Set Coords



---- PUZZLES ----


calculateMinRisk : BaseRisks -> Maybe Risk
calculateMinRisk baseRisks =
    let
        ( Matrix.Width width, Matrix.Height height ) =
            baseRisks
                |> baseRisksToRisks
                |> Matrix.dimensions

        startCoords : Coords
        startCoords =
            ( 0, 0 )

        endCoords : Coords
        endCoords =
            ( width - 1, height - 1 )

        initialVisited : Visited
        initialVisited =
            Set.empty

        initialAccRisks : AccumulatedRisks
        initialAccRisks =
            baseRisks
                |> baseRisksToRisks
                |> Matrix.indexedMap
                    (\coords _ ->
                        if coords == startCoords then
                            Risk 0

                        else
                            maxRisk
                    )
                |> AccumulatedRisks

        initialQueue : Queue
        initialQueue =
            let
                emptyQueue : Queue
                emptyQueue =
                    PriorityQueue.empty (Tuple.second >> riskToInt)
            in
            initialAccRisks
                |> accumulatedRisksToRisks
                |> Matrix.get startCoords
                |> Maybe.map (\risk -> PriorityQueue.insert ( startCoords, risk ) emptyQueue)
                |> Maybe.withDefault emptyQueue

        helper : Queue -> Visited -> AccumulatedRisks -> Maybe Risk
        helper queue visited accRisks =
            let
                hasBeenVisited : Coords -> Bool
                hasBeenVisited coords =
                    Set.member coords visited

                visit : Coords -> Visited
                visit coords =
                    Set.insert coords visited
            in
            case PriorityQueue.head queue of
                Nothing ->
                    accRisks
                        |> accumulatedRisksToRisks
                        |> Matrix.get endCoords

                Just ( coords, risk ) ->
                    if hasBeenVisited coords then
                        helper (PriorityQueue.tail queue) visited accRisks

                    else
                        let
                            updatedVisited : Visited
                            updatedVisited =
                                visit coords

                            neighbors : List ( Coords, Risk )
                            neighbors =
                                baseRisks
                                    |> baseRisksToRisks
                                    |> Matrix.getNeighbors coords
                                    |> List.filter (\( neighborCoords, _ ) -> not <| hasBeenVisited neighborCoords)

                            ( updatedAccRisks, updatedQueue ) =
                                List.foldl
                                    (\( neighborCoords, neighborBaseRisk ) (( accRisksAcc, queueAcc ) as acc_) ->
                                        accRisksAcc
                                            |> accumulatedRisksToRisks
                                            |> Matrix.get neighborCoords
                                            |> Maybe.map
                                                (\neighborAccRisk ->
                                                    let
                                                        updatedRisk : Risk
                                                        updatedRisk =
                                                            riskMap2 min neighborAccRisk <| addRisks risk neighborBaseRisk
                                                    in
                                                    ( accumulatedRisksMap (Matrix.set neighborCoords updatedRisk) accRisksAcc
                                                    , PriorityQueue.insert ( neighborCoords, updatedRisk ) queueAcc
                                                    )
                                                )
                                            |> Maybe.withDefault acc_
                                    )
                                    ( accRisks, queue )
                                    neighbors
                        in
                        helper updatedQueue updatedVisited updatedAccRisks
    in
    helper initialQueue initialVisited initialAccRisks


puzzle1 : String -> Maybe Int
puzzle1 input =
    input
        |> parse
        |> calculateMinRisk
        |> Maybe.map riskToInt


puzzle2 : String -> Maybe Int
puzzle2 input =
    let
        rotateRisk : Risk -> Risk
        rotateRisk =
            riskMap <| \amount -> (amount - 1) |> modBy 9 |> (+) 1

        expandRisks : Int -> BaseRisks -> BaseRisks
        expandRisks factor (BaseRisks risks) =
            let
                startCoords : Coords
                startCoords =
                    ( 0, 0 )
            in
            risks
                |> Matrix.new ( factor, factor )
                |> Matrix.indexedMap
                    (\(( i, j ) as coords) tile ->
                        if coords == startCoords then
                            tile

                        else
                            Matrix.map (riskMap ((+) (i + j)) >> rotateRisk) tile
                    )
                |> Matrix.concat
                |> BaseRisks
    in
    input
        |> parse
        |> expandRisks 5
        |> calculateMinRisk
        |> Maybe.map riskToInt



-- minRiskDynamicProgramming : Risks -> Maybe Risk
-- minRiskDynamicProgramming grid =
--     let
--         ( Matrix.Width width, Matrix.Height height ) =
--             Matrix.dimensions grid
--         -- startCoords : Coords
--         -- startCoords =
--         --     ( 0, 0 )
--         endCoords : Coords
--         endCoords =
--             ( width - 1, height - 1 )
--         widthRange : List Int
--         widthRange =
--             List.range 1 (width - 1)
--         heightRange : List Int
--         heightRange =
--             List.range 1 (height - 1)
--         initialTable : Matrix Int
--         initialTable =
--             Matrix.map (always 0) grid
--         table2 : Matrix Int
--         table2 =
--             -- Matrix.set startCoords 0 initialTable
--             -- grid
--             --     |> Matrix.get startCoords
--             --     |> Maybe.map (\(Risk risk) -> Matrix.set startCoords risk initialTable)
--             --     |> Maybe.withDefault initialTable
--             initialTable
--         tableFilledColumn : Matrix Int
--         tableFilledColumn =
--             List.foldl
--                 (\j acc ->
--                     Maybe.map2
--                         (\(Risk currentRisk) risk -> Matrix.set ( 0, j ) (currentRisk + risk) acc)
--                         (Matrix.get ( 0, j ) grid)
--                         (Matrix.get ( 0, j - 1 ) acc)
--                         -- |> (\x ->
--                         --         if x == Nothing then
--                         --             Debug.log "got Nothing" x
--                         --         else
--                         --             x
--                         --    )
--                         |> Maybe.withDefault acc
--                 )
--                 table2
--                 heightRange
--         tableFilledRow : Matrix Int
--         tableFilledRow =
--             List.foldl
--                 (\i acc ->
--                     Maybe.map2
--                         (\(Risk currentRisk) risk -> Matrix.set ( i, 0 ) (currentRisk + risk) acc)
--                         (Matrix.get ( i, 0 ) grid)
--                         (Matrix.get ( i - 1, 0 ) acc)
--                         |> Maybe.withDefault acc
--                 )
--                 tableFilledColumn
--                 widthRange
--         filledTable : Matrix Int
--         filledTable =
--             List.foldl
--                 (\j acc ->
--                     List.foldl
--                         (\i acc_ ->
--                             let
--                                 _ =
--                                     Debug.log "coords" ( j, i )
--                             in
--                             Maybe.map3
--                                 (\(Risk currentRisk) risk1 risk2 ->
--                                     Matrix.set ( i, j ) (currentRisk + min risk1 risk2) acc_
--                                 )
--                                 (Matrix.get ( i, j ) grid)
--                                 (Matrix.get ( i - 1, j ) acc_)
--                                 (Matrix.get ( i, j - 1 ) acc_)
--                                 |> Maybe.withDefault acc_
--                         )
--                         acc
--                         widthRange
--                 )
--                 tableFilledRow
--                 heightRange
--         -- |> (\x ->
--         --         let
--         --             _ =
--         --                 x
--         --                     |> Matrix.foldlRows
--         --                         (\_ row y ->
--         --                             let
--         --                                 _ =
--         --                                     Debug.log "row" row
--         --                             in
--         --                             y
--         --                         )
--         --                         0
--         --         in
--         --         x
--         --    )
--     in
--     filledTable
--         |> Matrix.get endCoords
--         |> Maybe.map Risk
