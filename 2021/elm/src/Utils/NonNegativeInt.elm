module Utils.NonNegativeInt exposing (NonNegativeInt, fromInt, map, toInt)


type NonNegativeInt
    = NonNegativeInt Int


fromInt : Int -> Maybe NonNegativeInt
fromInt value =
    if value < 0 then
        Nothing

    else
        Just <| NonNegativeInt value


toInt : NonNegativeInt -> Int
toInt (NonNegativeInt value) =
    value


map : (Int -> Int) -> NonNegativeInt -> Maybe NonNegativeInt
map f (NonNegativeInt value) =
    fromInt <| f value
