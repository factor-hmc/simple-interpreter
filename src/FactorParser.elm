module FactorParser exposing (..)

import Char
import Dict
import Lang exposing (..)
import Parser exposing (..)
import Parser.Extras exposing (..)
import Set
import Util exposing (..)


lex : Parser a -> Parser a
lex p =
    succeed identity
        |. spaces
        |= p


num : Parser Literal
num =
    lex <|
        number
            { int = Just Int
            , hex = Just Int
            , octal = Just Int
            , binary = Just Int
            , float = Just Float
            }
            |. spaces


string : Parser String
string =
    let
        special =
            Set.fromList
                [ '"'
                , '\\'
                ]

        escapes =
            [ ( '"', "\"" )
            , ( '\\', "\\" )
            , ( 'n', "\n" )
            , ( 't', "\t" )
            ]

        escaped =
            succeed identity
                |. symbol "\\"
                |= oneOf
                    (escapes
                        |> List.map
                            (\( c, s ) -> succeed s |. symbol (String.fromChar c))
                    )
    in
    succeed identity
        |. symbol "\""
        |= loop
            []
            (\s ->
                oneOf
                    [ oneOf
                        [ chompIf (\c -> not <| Set.member c special)
                            |> getChompedString
                        , escaped
                        ]
                        |> map (\c -> Loop (c :: s))
                    , symbol "\""
                        |> map (\() -> Done (s |> List.reverse |> String.concat))
                    ]
            )


literal : Parser Literal
literal =
    lex <|
        oneOf
            [ num
            , string |> map String
            , succeed Array
                |= between
                    (symbol "{")
                    (symbol "}")
                    (lazy <| \() -> many literal)
            , succeed Quotation
                |= between
                    (symbol "[")
                    (symbol "]")
                    (lazy <| \() -> words)
            , succeed F |. keyword "f"
            , succeed T |. keyword "t"
            ]


effect_ : Parser Effect
effect_ =
    let
        var =
            variable
                { start = not << isWhitespace
                , inner = not << isWhitespace
                , reserved = Set.fromList [ "(", ")", "--" ]
                }
    in
    lex <|
        succeed Effect
            |. symbol "("
            |. spaces
            |= many var
            |. spaces
            |. symbol "--"
            |. spaces
            |= many var
            |. spaces
            |. symbol ")"


definition : Parser Word
definition =
    lex <|
        succeed Definition
            |. symbol ":"
            |. spaces
            |= word
            |. spaces
            |= effect_
            |. spaces
            |= words
            |. spaces
            |. symbol ";"


word : Parser String
word =
    lex <|
        variable
            { start = not << isWhitespace
            , inner = not << isWhitespace
            , reserved = Set.fromList [ "]", "[", "}", "{", ":", ";" ]
            }
            |. spaces


words : Parser (List Word)
words =
    many
        (oneOf
            [ map (Builtin << Push) literal
            , map Word word
            , lazy <| \() -> definition
            ]
        )
        |. spaces
