module Day16 exposing (puzzle1, puzzle2)

import Binary
import Parser exposing ((|=), Parser)



---- BIT LENGTH ----


type BitLength
    = BitLength Int


bitLengthMap2 : (Int -> Int -> Int) -> BitLength -> BitLength -> BitLength
bitLengthMap2 f (BitLength length1) (BitLength length2) =
    BitLength <| f length1 length2


addBitLengths : BitLength -> BitLength -> BitLength
addBitLengths =
    bitLengthMap2 (+)


subtractBitLengths : BitLength -> BitLength -> BitLength
subtractBitLengths =
    bitLengthMap2 (-)


bitLengthToInt : BitLength -> Int
bitLengthToInt (BitLength length) =
    length



---- PACKETS ----


type PacketId
    = LiteralId
    | OperatorId OperatorType
    | Unknown


type OperatorType
    = OperatorSum
    | OperatorProduct
    | OperatorMinimum
    | OperatorMaximum
    | OperatorGreaterThan
    | OperatorLessThan
    | OperatorEqual


type OperatorLengthType
    = OperatorLength
    | OperatorNumPackets


type Packet
    = Literal Int
    | Operator OperatorType OperatorLengthType (List VersionedPacket)


type alias RawPacket =
    String


type alias VersionedPacket =
    { version : Int
    , rawPacket : RawPacket
    , packet : Packet
    }


versionedPacketBitLength : VersionedPacket -> BitLength
versionedPacketBitLength { rawPacket, packet } =
    case packet of
        Literal _ ->
            BitLength <| String.length rawPacket

        Operator _ _ subVersionedPackets ->
            List.foldl
                (versionedPacketBitLength >> addBitLengths)
                (BitLength <| String.length rawPacket)
                subVersionedPackets


versionBitLength : BitLength
versionBitLength =
    BitLength 3


typeIdBitLength : BitLength
typeIdBitLength =
    BitLength 3


literalValueStartBitLength : BitLength
literalValueStartBitLength =
    BitLength 1


literalValueBitLength : BitLength
literalValueBitLength =
    BitLength 4


operatorStartBitLength : BitLength
operatorStartBitLength =
    BitLength 1


operatorByLengthBitLength : BitLength
operatorByLengthBitLength =
    BitLength 15


operatorByNumPacketsBitLength : BitLength
operatorByNumPacketsBitLength =
    BitLength 11


parseHex : String -> Maybe String
parseHex =
    let
        hexParser : Parser String
        hexParser =
            Parser.loop [] <|
                \acc ->
                    Parser.oneOf
                        [ Parser.chompIf Char.isHexDigit
                            |> Parser.getChompedString
                            |> Parser.map (Binary.fromHex >> Binary.toIntegers >> List.map String.fromInt >> String.join "")
                            |> Parser.map (\bits -> Parser.Loop <| bits :: acc)
                        , Parser.succeed ()
                            |> Parser.map (\_ -> Parser.Done <| String.join "" <| List.reverse acc)
                        ]
    in
    Parser.run hexParser
        >> Result.toMaybe


parser : Parser VersionedPacket
parser =
    let
        isBit : Char -> Bool
        isBit char =
            char == '1' || char == '0'

        bitParser : Parser String
        bitParser =
            Parser.chompIf isBit
                |> Parser.getChompedString

        rawBinaryParserOfSize : BitLength -> Parser RawPacket
        rawBinaryParserOfSize (BitLength size) =
            Parser.loop ( size, [] ) <|
                \( size_, acc ) ->
                    let
                        rawPacket : RawPacket
                        rawPacket =
                            acc
                                |> List.reverse
                                |> String.join ""
                    in
                    if size_ <= 0 then
                        Parser.succeed <| Parser.Done rawPacket

                    else
                        Parser.map
                            (\bit -> Parser.Loop ( size_ - 1, bit :: acc ))
                            bitParser

        binaryParserOfSize : BitLength -> Parser ( RawPacket, Int )
        binaryParserOfSize =
            rawBinaryParserOfSize
                >> Parser.map
                    (\rawPacket ->
                        ( rawPacket
                        , rawPacket
                            |> Binary.fromString 1
                            |> Binary.toDecimal
                        )
                    )

        packetIdParser : Parser ( RawPacket, PacketId )
        packetIdParser =
            binaryParserOfSize typeIdBitLength
                |> Parser.map
                    (Tuple.mapSecond <|
                        \id ->
                            case id of
                                0 ->
                                    OperatorId OperatorSum

                                1 ->
                                    OperatorId OperatorProduct

                                2 ->
                                    OperatorId OperatorMinimum

                                3 ->
                                    OperatorId OperatorMaximum

                                4 ->
                                    LiteralId

                                5 ->
                                    OperatorId OperatorGreaterThan

                                6 ->
                                    OperatorId OperatorLessThan

                                7 ->
                                    OperatorId OperatorEqual

                                _ ->
                                    Unknown
                    )

        literalParser : Parser ( RawPacket, Packet )
        literalParser =
            Parser.succeed (Tuple.mapSecond Literal)
                |= valueParser

        valueParser : Parser ( RawPacket, Int )
        valueParser =
            Parser.loop [] <|
                \acc ->
                    Parser.succeed
                        (\( rawPacket1, startBit ) rawPacket2 ->
                            let
                                updatedAcc : List ( RawPacket, RawPacket )
                                updatedAcc =
                                    ( rawPacket1 ++ rawPacket2, rawPacket2 ) :: acc
                            in
                            if startBit == 1 then
                                Parser.Loop updatedAcc

                            else
                                updatedAcc
                                    |> List.foldl
                                        (\( allRawPacket, rawPacket ) ( allRawPacketAcc, rawPacketAcc ) ->
                                            ( allRawPacket ++ allRawPacketAcc
                                            , rawPacket ++ rawPacketAcc
                                            )
                                        )
                                        ( "", "" )
                                    |> Tuple.mapSecond (Binary.fromString 1 >> Binary.toDecimal)
                                    |> Parser.Done
                        )
                        |= binaryParserOfSize literalValueStartBitLength
                        |= rawBinaryParserOfSize literalValueBitLength

        operatorParser : OperatorType -> Parser ( RawPacket, Packet )
        operatorParser operatorType =
            let
                constructor : OperatorLengthType -> List VersionedPacket -> Packet
                constructor =
                    Operator operatorType
            in
            binaryParserOfSize operatorStartBitLength
                |> Parser.map
                    (Tuple.mapSecond <|
                        \lengthId ->
                            if lengthId == 0 then
                                OperatorLength

                            else
                                OperatorNumPackets
                    )
                |> Parser.andThen
                    (\( rawPacket1, lengthType ) ->
                        case lengthType of
                            OperatorLength ->
                                binaryParserOfSize operatorByLengthBitLength
                                    |> Parser.andThen
                                        (\( rawPacket2, length ) ->
                                            Parser.loop ( BitLength length, [] ) <|
                                                \( remainingLength, acc ) ->
                                                    if bitLengthToInt remainingLength <= 0 then
                                                        Parser.lazy <|
                                                            \_ ->
                                                                Parser.succeed <|
                                                                    Parser.Done
                                                                        ( rawPacket1 ++ rawPacket2
                                                                        , List.reverse acc
                                                                        )

                                                    else
                                                        Parser.map
                                                            (\subpacket ->
                                                                Parser.Loop
                                                                    ( subpacket
                                                                        |> versionedPacketBitLength
                                                                        |> subtractBitLengths remainingLength
                                                                    , subpacket :: acc
                                                                    )
                                                            )
                                                            parser
                                        )
                                    |> Parser.map (Tuple.mapSecond <| constructor lengthType)

                            OperatorNumPackets ->
                                binaryParserOfSize operatorByNumPacketsBitLength
                                    |> Parser.andThen
                                        (\( rawPacket2, numSubPackets ) ->
                                            Parser.loop ( numSubPackets, [] ) <|
                                                \( remainingPackets, acc ) ->
                                                    if remainingPackets <= 0 then
                                                        Parser.lazy <|
                                                            \_ ->
                                                                Parser.succeed <|
                                                                    Parser.Done
                                                                        ( rawPacket1 ++ rawPacket2
                                                                        , List.reverse acc
                                                                        )

                                                    else
                                                        Parser.map
                                                            (\subpacket ->
                                                                Parser.Loop
                                                                    ( remainingPackets - 1
                                                                    , subpacket :: acc
                                                                    )
                                                            )
                                                            parser
                                        )
                                    |> Parser.map (Tuple.mapSecond <| constructor lengthType)
                    )
    in
    Parser.succeed Tuple.pair
        |= binaryParserOfSize versionBitLength
        |= packetIdParser
        |> Parser.andThen
            (\( ( rawPacket1, version ), ( rawPacket2, packetId ) ) ->
                let
                    baseParser : Parser (( RawPacket, Packet ) -> VersionedPacket)
                    baseParser =
                        Parser.succeed <|
                            \( rawPacket3, packet ) ->
                                VersionedPacket version (rawPacket1 ++ rawPacket2 ++ rawPacket3) packet
                in
                case packetId of
                    LiteralId ->
                        baseParser
                            |= literalParser

                    OperatorId operatorType ->
                        baseParser
                            |= operatorParser operatorType

                    Unknown ->
                        Parser.problem "encountered unknown packetId"
            )


parse : String -> Maybe VersionedPacket
parse input =
    input
        |> String.trim
        |> parseHex
        |> Maybe.andThen (Parser.run parser >> Result.toMaybe)



---- PUZZLES ----


puzzle1 : String -> Maybe Int
puzzle1 input =
    let
        sumVersions : VersionedPacket -> Int
        sumVersions { version, packet } =
            case packet of
                Literal _ ->
                    version

                Operator _ _ subVersionedPackets ->
                    List.foldl (sumVersions >> (+)) version subVersionedPackets
    in
    input
        |> parse
        |> Maybe.map sumVersions


puzzle2 : String -> Maybe Int
puzzle2 input =
    let
        applyMathOperation : (Int -> Int -> Int) -> Int -> List VersionedPacket -> Maybe Int
        applyMathOperation f init subVersionedPackets =
            List.foldl (evaluate >> Maybe.map2 f) (Just init) subVersionedPackets

        applyNarrowing : (List Int -> Maybe Int) -> List VersionedPacket -> Maybe Int
        applyNarrowing f subVersionedPackets =
            subVersionedPackets
                |> List.filterMap evaluate
                |> f

        applyComparison : (Int -> Int -> Bool) -> List VersionedPacket -> Maybe Int
        applyComparison f subVersionedPackets =
            case subVersionedPackets |> List.map evaluate of
                [ Just value1, Just value2 ] ->
                    Just <|
                        if f value1 value2 then
                            1

                        else
                            0

                _ ->
                    Nothing

        evaluate : VersionedPacket -> Maybe Int
        evaluate { packet } =
            case packet of
                Literal value ->
                    Just value

                Operator operatorType _ subVersionedPackets ->
                    case operatorType of
                        OperatorSum ->
                            applyMathOperation (+) 0 subVersionedPackets

                        OperatorProduct ->
                            applyMathOperation (*) 1 subVersionedPackets

                        OperatorMinimum ->
                            applyNarrowing List.minimum subVersionedPackets

                        OperatorMaximum ->
                            applyNarrowing List.maximum subVersionedPackets

                        OperatorGreaterThan ->
                            applyComparison (>) subVersionedPackets

                        OperatorLessThan ->
                            applyComparison (<) subVersionedPackets

                        OperatorEqual ->
                            applyComparison (==) subVersionedPackets
    in
    input
        |> parse
        |> Maybe.andThen evaluate



---- OLD ----
-- 110 000  0 000000001010100 000000000000000001011000010001010110100010111000001000000000101111000110000010001101 000000
-- 110 000  0 000000001010100 000 000  0 000000000010110 000 100 0 1010 110 100 0 1011 100 000 1 00000000010 111 100 0 1100 000 100 0 1101 000000
--   6   0 15              84   0   0 15              22   0   4 0   10   6   4 0   11   4   0 1           2   7   4 0   12   0   4 0   13 000000
--
-- logWithLevel tag thing = logInfo ("level=" ++ String.fromInt level ++ " " ++ tag) thing
-- >> (\result ->
--         let
--             _ =
--                 result
--                     -- |> Result.mapError Parser.deadEndsToString
--                     |> logInfo "result"
--         in
--         result
--    )
--
-- headerBitLength : BitLength
-- headerBitLength =
--     addBitLengths versionBitLength typeIdBitLength
-- bitLengthToString : BitLength -> String
-- bitLengthToString =
--     bitLengthToInt >> String.fromInt
-- bitLengthMap : (Int -> Int) -> BitLength -> BitLength
-- bitLengthMap f (BitLength length) =
--     BitLength <| f length
-- bitLengthMap3 : (Int -> Int -> Int -> Int) -> BitLength -> BitLength -> BitLength -> BitLength
-- bitLengthMap3 f (BitLength length1) (BitLength length2) (BitLength length3) =
--     BitLength <| f length1 length2 length3
-- literalValueWithIndicatorBitLength : BitLength
-- literalValueWithIndicatorBitLength =
--     bitLengthMap ((+) 1) literalValueBitLength
--
-- roundUpToNearestMultipleOf : Int -> Int -> Int
-- roundUpToNearestMultipleOf factor value =
--     let
--         sum =
--             value + factor - 1
--     in
--     sum - modBy factor sum
--
-- t : String -> List String -> String
-- t template vars =
--     template
--         |> String.split "{}"
--         |> List.Extra.zip vars
--         |> List.map (\( b, a ) -> a ++ b)
--         |> String.join ""
-- type LengthStrategy
--     = RoundedTo BitLength
--     | NoRounding
-- versionedPacketBitLengthOld : LengthStrategy -> VersionedPacket -> BitLength
-- versionedPacketBitLengthOld lengthStrategy { packet } =
--     case packet of
--         Literal value ->
--             let
--                 unpaddedValueLength : Int
--                 unpaddedValueLength =
--                     value
--                         |> Binary.fromDecimal
--                         |> Binary.toIntegers
--                         |> List.length
--                         |> logInfo "unpaddedValueLength"
--                 paddedValueLength : Int
--                 paddedValueLength =
--                     roundUpToNearestMultipleOf (bitLengthToInt literalValueBitLength) unpaddedValueLength
--                         |> logInfo "paddedValueLength"
--                 numValueBits : Int
--                 numValueBits =
--                     (paddedValueLength // bitLengthToInt literalValueBitLength)
--                         |> logInfo "numValueBits"
--                 baseLength : BitLength
--                 baseLength =
--                     literalValueWithIndicatorBitLength
--                         |> bitLengthMap ((*) numValueBits)
--                         |> addBitLengths headerBitLength
--                         |> logInfo "baseLength"
--             in
--             case lengthStrategy of
--                 RoundedTo bitLength -> bitLengthMap (roundUpToNearestMultipleOf <| bitLengthToInt bitLength) baseLength
--                 NoRounding -> baseLength
--         Operator lengthType subVersionedPackets ->
--             let
--                 lengthTypeLength : BitLength
--                 lengthTypeLength =
--                     BitLength <|
--                         case lengthType of
--                             OperatorLength ->
--                                 16
--                             OperatorNumPackets ->
--                                 12
--             in
--             subVersionedPackets
--                 |> List.foldl
--                     -- (\subVersionedPacket acc -> acc |> addBitLengths (versionedPacketBitLength subVersionedPacket))
--                     (versionedPacketBitLength lengthStrategy >> addBitLengths)
--                     (BitLength 0)
--                 |> addBitLengths headerBitLength
--                 |> addBitLengths lengthTypeLength
