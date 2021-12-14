module Utils.Tuple2 exposing (fromList, sequenceMaybe)


fromList : List a -> Maybe ( a, a )
fromList list =
    case list of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


sequenceMaybe : ( Maybe a, Maybe b ) -> Maybe ( a, b )
sequenceMaybe pair =
    case pair of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing
