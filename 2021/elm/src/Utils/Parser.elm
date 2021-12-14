module Utils.Parser exposing (justSpaces, newlineOrEnd)

import Parser exposing (Parser)


newlineOrEnd : Parser ()
newlineOrEnd =
    Parser.oneOf
        [ Parser.chompIf ((==) '\n')
        , Parser.end
        ]
