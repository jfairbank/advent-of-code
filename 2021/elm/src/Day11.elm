module Day11 exposing (puzzle1, puzzle2)

import NaiveMatrix as Matrix exposing (Coords, Matrix)
import Set exposing (Set)


type Energy
    = Energy Int


mapEnergy : (Int -> Int) -> Energy -> Energy
mapEnergy f (Energy energy) =
    Energy <| f energy


isReadyToFlash : Energy -> Bool
isReadyToFlash (Energy energy) =
    energy > 9


type alias Grid =
    Matrix Energy


parse : String -> Grid
parse input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.map (String.split "" >> List.filterMap (String.toInt >> Maybe.map Energy))
        |> Matrix.fromList


getNeighbors : Coords -> Grid -> List ( Coords, Energy )
getNeighbors ( i, j ) grid =
    [ ( i - 1, j ) --       top
    , ( i - 1, j + 1 ) --   top right
    , ( i, j + 1 ) --       right
    , ( i + 1, j + 1 ) --   bottom right
    , ( i + 1, j ) --       bottom
    , ( i + 1, j - 1 ) --   bottom left
    , ( i, j - 1 ) --       left
    , ( i - 1, j - 1 ) --   top left
    ]
        |> List.filterMap
            (\coords ->
                grid
                    |> Matrix.get coords
                    |> Maybe.map (\neighbor -> ( coords, neighbor ))
            )



---- PUZZLES ----


runStep : Grid -> ( Int, Grid )
runStep grid =
    let
        increasedGrid : Grid
        increasedGrid =
            Matrix.map (mapEnergy ((+) 1)) grid

        readyToFlash : List Coords
        readyToFlash =
            increasedGrid
                |> Matrix.foldl
                    (\coords energy acc ->
                        if isReadyToFlash energy then
                            coords :: acc

                        else
                            acc
                    )
                    []
                |> List.reverse

        flash : List Coords -> Set Coords -> Grid -> ( Set Coords, Grid )
        flash flashQueue flashed grid_ =
            case flashQueue of
                coords :: remaining ->
                    let
                        updatedFlashed : Set Coords
                        updatedFlashed =
                            Set.insert coords flashed

                        neighbors : List ( Coords, Energy )
                        neighbors =
                            grid_
                                |> getNeighbors coords
                                |> List.filter
                                    (\( neighborCoords, neighborEnergy ) ->
                                        (not <| isReadyToFlash neighborEnergy)
                                            && (not <| Set.member neighborCoords flashed)
                                    )

                        updatedNeighbors : List ( Coords, Energy )
                        updatedNeighbors =
                            List.map (Tuple.mapSecond <| mapEnergy ((+) 1)) neighbors

                        updatedGrid : Grid
                        updatedGrid =
                            List.foldl
                                (\( neighborCoords, neighborEnergy ) accGrid ->
                                    Matrix.set neighborCoords neighborEnergy accGrid
                                )
                                (Matrix.set coords (Energy 0) grid_)
                                updatedNeighbors

                        neighborsForQueue : List Coords
                        neighborsForQueue =
                            List.filterMap
                                (\( neighborCoords, neighborEnergy ) ->
                                    if isReadyToFlash neighborEnergy then
                                        Just neighborCoords

                                    else
                                        Nothing
                                )
                                updatedNeighbors

                        updatedQueue : List Coords
                        updatedQueue =
                            neighborsForQueue ++ remaining
                    in
                    flash updatedQueue updatedFlashed updatedGrid

                [] ->
                    ( flashed, grid_ )
    in
    flash readyToFlash Set.empty increasedGrid
        |> Tuple.mapFirst Set.size


puzzle1 : String -> Int
puzzle1 input =
    let
        grid : Grid
        grid =
            parse input

        numSteps : Int
        numSteps =
            100
    in
    List.range 1 numSteps
        |> List.foldl
            (\_ ( flashCount, accGrid ) ->
                runStep accGrid
                    |> Tuple.mapFirst ((+) flashCount)
            )
            ( 0, grid )
        |> Tuple.first


puzzle2 : String -> Int
puzzle2 input =
    let
        helper : Int -> Grid -> Int
        helper step grid =
            let
                ( numFlashes, updatedGrid ) =
                    runStep grid
            in
            if numFlashes == 100 then
                step

            else
                helper (step + 1) updatedGrid
    in
    input
        |> parse
        |> helper 1
