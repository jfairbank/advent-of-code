module Stack exposing (Stack, empty, insert, isEmpty, pop, toList)


type Stack a
    = Stack (List a)


empty : Stack a
empty =
    Stack []


isEmpty : Stack a -> Bool
isEmpty (Stack list) =
    List.isEmpty list


insert : a -> Stack a -> Stack a
insert value (Stack list) =
    Stack (value :: list)


pop : Stack a -> Maybe ( a, Stack a )
pop (Stack list) =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, Stack xs )


toList : Stack a -> List a
toList (Stack list) =
    list
