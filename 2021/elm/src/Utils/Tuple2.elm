module Utils.Tuple2 exposing (sequenceMaybe)


sequenceMaybe : ( Maybe a, Maybe b ) -> Maybe ( a, b )
sequenceMaybe pair =
    case pair of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing
