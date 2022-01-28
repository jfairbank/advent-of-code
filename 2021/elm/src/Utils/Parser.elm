module Utils.Parser exposing (justSpaces, newline, newlineOrEnd, repeated, signedInt)

import Parser exposing ((|.), (|=), Parser)


repeated : Int -> Parser a -> Parser (List a)
repeated n parser =
    Parser.loop ( n, [] ) <|
        \( count, pieces ) ->
            if count <= 0 then
                Parser.lazy (\_ -> Parser.succeed <| Parser.Done <| List.reverse pieces)

            else
                Parser.map (\piece -> Parser.Loop ( count - 1, piece :: pieces )) parser


justSpaces : Parser ()
justSpaces =
    Parser.chompWhile ((==) ' ')


newline : Parser ()
newline =
    Parser.chompIf ((==) '\n')


newlineOrEnd : Parser ()
newlineOrEnd =
    Parser.oneOf
        [ newline
        , Parser.end
        ]


signedInt : Parser Int
signedInt =
    Parser.oneOf
        [ Parser.int
        , Parser.succeed negate
            |. Parser.backtrackable (Parser.symbol "-")
            |= Parser.oneOf
                [ Parser.backtrackable Parser.int
                    |> Parser.andThen Parser.commit
                , Parser.problem "expecting negative int"
                ]
        ]
