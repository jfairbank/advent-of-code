module Day18 exposing (puzzle1, puzzle2)

import Array exposing (Array)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Utils.Parser


type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a


type Level
    = Level Int


type Index
    = Index Int


type alias SnailfishTree =
    Tree Int


type alias IndexedSnailfishTree =
    Tree ( Index, Int )


snailfishTreeToString : SnailfishTree -> String
snailfishTreeToString tree =
    case tree of
        Node left right ->
            "[" ++ snailfishTreeToString left ++ "," ++ snailfishTreeToString right ++ "]"

        Leaf value ->
            String.fromInt value


indexedSnailfishTreeToString : IndexedSnailfishTree -> String
indexedSnailfishTreeToString tree =
    case tree of
        Node left right ->
            "[" ++ indexedSnailfishTreeToString left ++ "," ++ indexedSnailfishTreeToString right ++ "]"

        Leaf ( _, value ) ->
            String.fromInt value


logSnailfishTree : String -> SnailfishTree -> SnailfishTree
logSnailfishTree tag tree =
    let
        _ =
            Debug.log tag <| snailfishTreeToString tree
    in
    tree


logIndexedSnailfishTree : String -> IndexedSnailfishTree -> IndexedSnailfishTree
logIndexedSnailfishTree tag tree =
    let
        _ =
            Debug.log tag <| indexedSnailfishTreeToString tree
    in
    tree


lineParser : Parser SnailfishTree
lineParser =
    Parser.oneOf
        [ Parser.succeed Leaf
            |= Parser.int
        , Parser.succeed Node
            |. Parser.symbol "["
            |= Parser.lazy (\_ -> lineParser)
            |. Parser.symbol ","
            |= Parser.lazy (\_ -> lineParser)
            |. Parser.symbol "]"
        ]


parse : String -> List SnailfishTree
parse input =
    input
        |> String.trim
        |> String.lines
        |> List.filterMap (String.trim >> Parser.run lineParser >> Result.toMaybe)



---- PUZZLES ----


add : Tree a -> Tree a -> Tree a
add left right =
    Node left right


magnitude : SnailfishTree -> Int
magnitude tree =
    case tree of
        Leaf value ->
            value

        Node left right ->
            3 * magnitude left + 2 * magnitude right


addLeft : Int -> SnailfishTree -> SnailfishTree
addLeft value tree =
    case tree of
        Node left right ->
            Node (addLeft value left) right

        Leaf otherValue ->
            Leaf <| value + otherValue


addRight : Int -> SnailfishTree -> SnailfishTree
addRight value tree =
    case tree of
        Node left right ->
            Node left <| addRight value right

        Leaf otherValue ->
            Leaf <| value + otherValue



-- type Changeable a
--     = Changed a
--     | Unchanged a
-- mapChangeable : (a -> b) -> Changeable a -> Changeable b
-- mapChangeable f changeable =
--     case changeable of
--         Changed value ->
--             Changed <| f value
--         Unchanged value ->
--             Unchanged <| f value
-- unwrapChangeable : Changeable a -> a
-- unwrapChangeable changeable =
--     case changeable of
--         Changed value ->
--             value
--         Unchanged value ->
--             value
-- mapChanged : (a -> a) -> Changeable a -> Changeable a
-- mapChanged f changeable =
--     case changeable of
--         Changed value ->
--             Changed <| f value
--         Unchanged _ ->
--             changeable


explode : SnailfishTree -> Maybe SnailfishTree
explode =
    let
        helper : Level -> SnailfishTree -> Maybe ( SnailfishTree, ( Int, Int ) )
        helper (Level level) tree =
            case ( level >= 4, tree ) of
                ( True, Node (Leaf leftValue) (Leaf rightValue) ) ->
                    Just
                        ( Leaf 0
                        , ( leftValue, rightValue )
                        )

                ( _, Node left right ) ->
                    let
                        nextLevel : Level
                        nextLevel =
                            Level <| level + 1
                    in
                    case helper nextLevel left of
                        Just ( updatedLeft, ( leftValue, rightValue ) ) ->
                            Just
                                ( Node updatedLeft <| addLeft rightValue right
                                , ( leftValue, 0 )
                                )

                        Nothing ->
                            right
                                |> helper nextLevel
                                |> Maybe.map
                                    (\( updatedRight, ( leftValue, rightValue ) ) ->
                                        ( Node (addRight leftValue left) updatedRight
                                        , ( 0, rightValue )
                                        )
                                    )

                ( _, Leaf _ ) ->
                    Nothing
    in
    helper (Level 0)
        >> Maybe.map Tuple.first


split : SnailfishTree -> Maybe SnailfishTree
split tree =
    case tree of
        Leaf value ->
            if value >= 10 then
                Just <|
                    Node
                        (Leaf <| value // 2)
                        (Leaf <| (value + 1) // 2)

            else
                Nothing

        Node left right ->
            case split left of
                Just updatedLeft ->
                    Just <| Node updatedLeft right

                Nothing ->
                    right
                        |> split
                        |> Maybe.map (\updatedRight -> Node left updatedRight)


reduce : SnailfishTree -> SnailfishTree
reduce tree =
    case explode tree of
        Just updatedTree ->
            reduce updatedTree

        Nothing ->
            case split tree of
                Just updatedTree ->
                    reduce updatedTree

                Nothing ->
                    tree


puzzle1 : String -> Maybe Int
puzzle1 input =
    input
        |> parse
        |> List.foldl
            (\tree acc ->
                case acc of
                    Nothing ->
                        Just tree

                    Just prevTree ->
                        add prevTree tree
                            |> reduce
                            |> Just
            )
            Nothing
        |> Maybe.map magnitude


puzzle2 : String -> Maybe Int
puzzle2 input =
    let
        lines : List SnailfishTree
        lines =
            parse input
    in
    lines
        |> List.concatMap (\line -> List.map (Tuple.pair line) lines)
        |> List.map
            (\( x, y ) ->
                -- if x == y then
                add x y
                    |> reduce
                    |> magnitude
            )
        |> List.maximum



-- inputs : List String
-- inputs =
--     -- [ "[1,2]"
--     -- , "[[1,2],3]"
--     -- , "[9,[8,7]]"
--     -- , "[[1,9],[8,5]]"
--     -- , "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
--     -- , "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
--     -- , "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
--     [ "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
--     , "[[[[[9,8],1],2],3],4]"
--     , "[7,[6,[5,[4,[3,2]]]]]"
--     , "[[6,[5,[4,[3,2]]]],1]"
--     , "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
--     , "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
--     ]
---- OLD ----
-- addFromLeft value tree =
--     case tree of
--         Leaf otherValue -> Leaf <| value + otherValue
--         Node left _ -> addFromLeft value left
-- addFromRight value tree =
--     case tree of
--         Leaf otherValue -> Leaf <| value + otherValue
--         Node _ right -> addFromRight value right
-- explode : Level -> SnailfishTree -> Maybe (SnailfishTree, Maybe (Maybe Int, Maybe Int))
-- explode (Level level) tree =
--     case ( level, tree ) of
--         ( 4, Node (Leaf leftValue) (Leaf rightValue) ) ->
--             Just ( Leaf 0, Just ( Just leftValue, Just rightValue ) )
--         ( _, Node left right ) ->
--             let
--                 ( updatedLeft, maybeLeftPair ) =
--                     explode (Level <| level + 1) left
--                 ( updatedRight, maybeRightPair ) =
--                     explode (Level <| level + 1) right
--             in
--             case maybeLeftPair of
--                 Just ( maybeLeftPairLeftValue, Just leftPairRightValue ) ->
--                     ( Node updatedLeft (addFromLeft leftPairRightValue right)
--                     , Just ( maybeLeftPairLeftValue, Nothing )
--                     )
--                 Just ( maybeLeftPairLeftValue, Nothing ) ->
--                     ( Node updatedLeft right
--                     , Just ( maybeLeftPairLeftValue, Nothing )
--                     )
--                 Nothing ->
--                     case maybeRightPair of
--                         Just ( Just rightPairLeftValue, maybeRightPairRightValue ) ->
--                             ( Node (addFromRight rightPairLeftValue left) updatedRight
--                             , Just ( Nothing, maybeRightPairRightValue )
--                             )
--                         Just ( Nothing, maybeRightPairRightValue ) ->
--                             ( Node left updatedRight
--                             , Just ( Nothing, maybeRightPairRightValue )
--                             )
--                         Nothing ->
--                             ( tree, Nothing )
--         _ ->
--             ( tree, Nothing )
-- at : Int -> List a -> Maybe a
-- at index list =
--     if index < 0 then
--         Nothing
--     else
--         case list of
--             [] ->
--                 Nothing
--             x :: xs ->
--                 if index == 0 then
--                     Just x
--                 else
--                     at (index - 1) xs
-- updateFromExplodedPair : ( ( Index, Int ), ( Index, Int ) ) -> IndexedSnailfishTree -> IndexedSnailfishTree
-- updateFromExplodedPair ( ( leftIndex, leftValue ), ( rightIndex, rightValue ) ) tree =
--     tree
--         |> updateIndexedSnailfishTreeAt (leftIndex |> indexMap decrement) ((+) leftValue)
--         |> updateIndexedSnailfishTreeAt (rightIndex |> indexMap increment) ((+) rightValue)
--         |> updatePairInIndexedSnailfishTreeAt leftIndex (always <| Leaf ( leftIndex, 0 ))
--         |> reindexSnailfishTree
-- identifyExplodingPair : Level -> IndexedSnailfishTree -> Maybe ( ( Index, Int ), ( Index, Int ) )
-- identifyExplodingPair (Level level) tree =
--     case ( level, tree ) of
--         ( 4, Node (Leaf ( leftIndex, leftValue )) (Leaf ( rightIndex, rightValue )) ) ->
--             Just ( ( leftIndex, leftValue ), ( rightIndex, rightValue ) )
--         ( _, Node left right ) ->
--             let
--                 nextLevel =
--                     Level <| level + 1
--             in
--             Maybe.Extra.or
--                 (identifyExplodingPair nextLevel left)
--                 (identifyExplodingPair nextLevel right)
--         ( _, Leaf _ ) ->
--             Nothing
-- split : Changed SnailfishTree -> Changed SnailfishTree
-- split changedTree =
--     let
--         ( constructor, tree ) =
--             case changedTree of
--                 Unchanged tree_ ->
--                     ( Unchanged, tree_ )
--                 Changed tree_ ->
--                     ( Changed, tree_ )
--     in
--     case tree of
--         Leaf value ->
--             if value >= 10 then
--                 let
--                     halved =
--                         value // 2
--                     updatedLeft =
--                         Leaf halved
--                     updatedRight =
--                         Leaf <| halved + remainderBy 2 value
--                 in
--                 Changed <| Node updatedLeft updatedRight
--             else
--                 constructor tree
--         Node left right ->
--             case split <| constructor left of
--                 Changed updatedLeft ->
--                     Changed <| Node updatedLeft right
--                 Unchanged _ ->
--                     case split <| constructor right of
--                         Changed updatedRight ->
--                             Changed <| Node left updatedRight
--                         Unchanged _ ->
--                             Unchanged tree
-- helper : IndexedSnailfishTree -> IndexedSnailfishTree
-- helper tree =
--     case identifyExplodingPair (Level 0) tree of
--         Just pair ->
--             helper (updateFromExplodedPair pair tree)
--         Nothing ->
--             case split <| Unchanged <| unindexSnailfishTree tree of
--                 Changed updatedTree ->
--                     helper (indexSnailfishTree updatedTree)
--                 Unchanged _ ->
--                     tree
-- ( _, Node left right ) ->
--     let
--         ( updatedLeft, maybeLeftPair ) =
--             explode2 (Level <| level + 1) left
--         ( updatedRight, maybeRightPair ) =
--             explode2 (Level <| level + 1) right
--     in
--     case maybeLeftPair of
--         Just ( maybeLeftPairLeftValue, Just leftPairRightValue ) ->
--             ( Node updatedLeft (addFromLeft leftPairRightValue right)
--             , Just ( maybeLeftPairLeftValue, Nothing )
--             )
--         Just ( maybeLeftPairLeftValue, Nothing ) ->
--             ( Node updatedLeft right
--             , Just ( maybeLeftPairLeftValue, Nothing )
--             )
--         Nothing ->
--             case maybeRightPair of
--                 Just ( Just rightPairLeftValue, maybeRightPairRightValue ) ->
--                     ( Node (addFromRight rightPairLeftValue left) updatedRight
--                     , Just ( Nothing, maybeRightPairRightValue )
--                     )
--                 Just ( Nothing, maybeRightPairRightValue ) ->
--                     ( Node left updatedRight
--                     , Just ( Nothing, maybeRightPairRightValue )
--                     )
--                 Nothing ->
--                     ( tree, Nothing )
-- treeMap : (a -> b) -> Tree a -> Tree b
-- treeMap f tree =
--     case tree of
--         Leaf value ->
--             Leaf <| f value
--         Node left right ->
--             Node (treeMap f left) (treeMap f right)
-- type Changed a
--     = Changed a
--     | Unchanged a
-- unwrapChanged : Changed a -> a
-- unwrapChanged changed =
--     case changed of
--         Changed value ->
--             value
--         Unchanged value ->
--             value
-- indexSnailfishTree : SnailfishTree -> IndexedSnailfishTree
-- indexSnailfishTree =
--     let
--         helper : Index -> SnailfishTree -> ( Index, IndexedSnailfishTree )
--         helper index tree =
--             case tree of
--                 Leaf value ->
--                     ( indexMap ((+) 1) index
--                     , Leaf ( index, value )
--                     )
--                 Node left right ->
--                     let
--                         ( updatedIndex1, updatedLeft ) =
--                             helper index left
--                         ( updatedIndex2, updatedRight ) =
--                             helper updatedIndex1 right
--                     in
--                     ( updatedIndex2, Node updatedLeft updatedRight )
--     in
--     helper (Index 0)
--         >> Tuple.second
-- unindexSnailfishTree : IndexedSnailfishTree -> SnailfishTree
-- unindexSnailfishTree =
--     treeMap Tuple.second
-- reindexSnailfishTree : IndexedSnailfishTree -> IndexedSnailfishTree
-- reindexSnailfishTree =
--     unindexSnailfishTree >> indexSnailfishTree
-- updateIndexedSnailfishTreeAt : Index -> (Int -> Int) -> IndexedSnailfishTree -> IndexedSnailfishTree
-- updateIndexedSnailfishTreeAt ((Index indexValue) as index) f tree =
--     if indexValue < 0 then
--         tree
--     else
--         case tree of
--             Leaf ( i, value ) ->
--                 if i == index then
--                     Leaf ( i, f value )
--                 else
--                     tree
--             Node left right ->
--                 let
--                     updatedLeft =
--                         updateIndexedSnailfishTreeAt index f left
--                     updatedRight =
--                         updateIndexedSnailfishTreeAt index f right
--                 in
--                 Node updatedLeft updatedRight
-- decrement : number -> number
-- decrement n =
--     n - 1
-- increment : number -> number
-- increment =
--     (+) 1
-- updatePairInIndexedSnailfishTreeAt : Index -> (IndexedSnailfishTree -> IndexedSnailfishTree) -> IndexedSnailfishTree -> IndexedSnailfishTree
-- updatePairInIndexedSnailfishTreeAt (Index index) f tree =
--     if index < 0 then
--         tree
--     else
--         case tree of
--             Node (Leaf ( Index i, _ )) (Leaf _) ->
--                 if i == index then
--                     f tree
--                 else
--                     tree
--             Leaf _ ->
--                 tree
--             Node left right ->
--                 let
--                     updatedLeft =
--                         updatePairInIndexedSnailfishTreeAt (Index index) f left
--                     updatedRight =
--                         updatePairInIndexedSnailfishTreeAt (Index index) f right
--                 in
--                 Node updatedLeft updatedRight
-- indexMap : (Int -> Int) -> Index -> Index
-- indexMap f (Index index) =
--     Index <| f index
