module Day09 exposing (puzzle1, puzzle2)

import NaiveMatrix as Matrix exposing (Coords, Matrix)
import Set exposing (Set)


type alias Grid =
    Matrix Int


parse : String -> Grid
parse input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.map (String.split "" >> List.filterMap String.toInt)
        |> Matrix.fromList


getNeighbors : Coords -> Grid -> List ( Coords, Int )
getNeighbors ( i, j ) grid =
    [ ( i - 1, j ) -- top
    , ( i, j + 1 ) -- right
    , ( i + 1, j ) -- bottom
    , ( i, j - 1 ) -- left
    ]
        |> List.filterMap
            (\coords ->
                grid
                    |> Matrix.get coords
                    |> Maybe.map (\neighbor -> ( coords, neighbor ))
            )


getLowPoints : Grid -> List ( Coords, Int )
getLowPoints grid =
    grid
        |> Matrix.foldl
            (\coords height acc ->
                let
                    isLowPoint =
                        grid
                            |> getNeighbors coords
                            |> List.all (\( _, neighbor ) -> height < neighbor)
                in
                if isLowPoint then
                    ( coords, height ) :: acc

                else
                    acc
            )
            []
        |> List.reverse



---- PUZZLES ----


puzzle1 : String -> Int
puzzle1 input =
    input
        |> parse
        |> getLowPoints
        |> List.foldl (\( _, height ) acc -> height + 1 + acc) 0


puzzle2 : String -> Int
puzzle2 input =
    let
        grid : Grid
        grid =
            parse input

        lowPoints : List ( Coords, Int )
        lowPoints =
            getLowPoints grid

        getBasin : List ( Coords, Int ) -> Set Coords -> List ( Coords, Int ) -> List ( Coords, Int )
        getBasin queue visited acc =
            case queue of
                [] ->
                    acc

                (( coords, _ ) as entry) :: rest ->
                    let
                        basinNeighbors =
                            grid
                                |> getNeighbors coords
                                |> List.filter
                                    (\( neighborCoords, neighborHeight ) ->
                                        neighborHeight /= 9 && (not <| Set.member neighborCoords visited)
                                    )

                        updatedVisited =
                            List.foldl (Tuple.first >> Set.insert) visited basinNeighbors
                                |> Set.insert coords
                    in
                    getBasin (basinNeighbors ++ rest) updatedVisited (entry :: acc)
    in
    lowPoints
        |> List.map
            (\entry ->
                getBasin [ entry ] Set.empty []
                    |> List.length
            )
        |> List.sortBy negate
        |> List.take 3
        |> List.product
