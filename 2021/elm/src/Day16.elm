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
