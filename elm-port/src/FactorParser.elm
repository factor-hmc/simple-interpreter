module FactorParser exposing (..)

import Char
import Set
import Util exposing (..)
import Lang exposing (..)
import Parser exposing (..)
import Parser.Extras exposing (..)

lex : Parser a -> Parser a
lex p = succeed identity
        |. spaces
        |= p

num : Parser Literal
num = lex <| number
      { int = Just Int
      , hex = Just Int
      , octal = Just Int
      , binary = Just Int
      , float = Just Float}
      |. spaces

literal : Parser Literal
literal = lex <| oneOf
          [ num
          , between (symbol "{") (symbol "}") (lazy <| \_ -> many literal) |> map Array
          , between (symbol "[") (symbol "]") (lazy <| \_ -> words) |> map Quotation
          ]

word : Parser Word
word = lex <| map Word <|
       variable
       { start    = not << isWhitespace
       , inner    = not << isWhitespace
       , reserved = Set.empty}
      |. spaces

words : Parser (List Word)
words = many
        (oneOf
        [ map (Builtin << Push) literal
        , word
        ])
        |. spaces
        |. end
