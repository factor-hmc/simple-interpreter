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
          , succeed F |. keyword "f"
          , succeed T |. keyword "t"
          ]

word : Parser Word
word = lex <| map Word <|
       variable
       { start    = not << isWhitespace
       , inner    = not << isWhitespace
       , reserved = Set.fromList ["]", "[", "}", "{"] }
      |. spaces

words : Parser (List Word)
words = many
        (oneOf
        [ map (Builtin << Push) literal
      
            ,succeed (Builtin  Add) |. keyword "+"
            ,succeed (Builtin  Mul) |. keyword "*"
            ,succeed (Builtin  Sub) |. keyword "-"
            ,succeed (Builtin  Div) |. keyword "/"
            ,succeed (Builtin  Eq) |. keyword "="
            ,succeed (Builtin  If) |. keyword "if"
            ,succeed (Builtin  Dup) |. keyword "dup"
            ,succeed (Builtin  Swap) |. keyword "swap"
            ,succeed (Builtin  Rot) |. keyword "rot"
            ,succeed (Builtin  Drop) |. keyword "drop"
            ,succeed (Builtin  While) |. keyword "while"
            ,succeed (Builtin  Clear) |. keyword "clear"

           {-} | Sub
            | Mul
            | Div
            | Eq
            | If
            | Dup
            | Swap
            | Rot
            | Drop
            | While
            | Clear
            -}
        
        , word
        ])
        |. spaces
