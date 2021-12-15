module Utils.NaiveMatrix exposing
    ( Coords
    , Matrix
    , Vector
    , filter
    , foldl
    , foldlRows
    , fromList
    , get
    , getColumn
    , getRow
    , height
    , map
    , new
    , set
    , width
    )

import Array exposing (Array)


type alias Vector a =
    Array a


type alias Coords =
    ( Int, Int )


type Matrix a
    = Matrix (Array (Vector a))


new : ( Int, Int ) -> a -> Matrix a
new ( width_, height_ ) initialValue =
    List.repeat height_ (List.repeat width_ initialValue)
        |> fromList


fromList : List (List a) -> Matrix a
fromList listMatrix =
    listMatrix
        |> List.map Array.fromList
        |> Array.fromList
        |> Matrix


height : Matrix a -> Int
height (Matrix matrix) =
    Array.length matrix


width : Matrix a -> Int
width (Matrix matrix) =
    matrix
        |> Array.get 0
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


filter : (Vector a -> Bool) -> Matrix a -> Matrix a
filter f (Matrix matrix) =
    matrix
        |> Array.filter f
        |> Matrix


foldlRows : (Int -> Array a -> b -> b) -> b -> Matrix a -> b
foldlRows f init (Matrix matrix) =
    matrix
        |> Array.foldl
            (\row ( index, acc ) -> ( index + 1, f index row acc ))
            ( 0, init )
        |> Tuple.second


foldl : (Coords -> a -> b -> b) -> b -> Matrix a -> b
foldl f =
    foldlRows
        (\j row acc ->
            row
                |> Array.foldl (\el ( i, acc_ ) -> ( i + 1, f ( i, j ) el acc_ )) ( 0, acc )
                |> Tuple.second
        )


map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix matrix) =
    matrix
        |> Array.map (Array.map f)
        |> Matrix


getRow : Int -> Matrix a -> Maybe (Vector a)
getRow key (Matrix matrix) =
    Array.get key matrix


getColumn : Int -> Matrix a -> Maybe (Vector a)
getColumn key =
    foldlRows
        (\_ row acc ->
            Array.get key row
                |> Maybe.map2 (\acc_ item -> Array.push item acc_) acc
        )
        (Just Array.empty)


get : Coords -> Matrix a -> Maybe a
get ( i, j ) (Matrix matrix) =
    matrix
        |> Array.get j
        |> Maybe.andThen (Array.get i)


set : Coords -> a -> Matrix a -> Matrix a
set ( i, j ) value ((Matrix matrix) as m) =
    matrix
        |> Array.get j
        |> Maybe.map (\row -> Matrix <| Array.set j (Array.set i value row) matrix)
        |> Maybe.withDefault m
