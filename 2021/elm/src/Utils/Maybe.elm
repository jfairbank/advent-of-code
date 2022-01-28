module Utils.Maybe exposing (withDefaultLazy)


withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy thunk maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            thunk ()
