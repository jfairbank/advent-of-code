module Utils.Parser exposing (justSpaces, newlineOrEnd)

import Parser exposing (Parser)


justSpaces : Parser ()
justSpaces =
    Parser.chompWhile ((==) ' ')


newlineOrEnd : Parser ()
newlineOrEnd =
    Parser.oneOf
        [ Parser.chompIf ((==) '\n')
        , Parser.end
        ]
