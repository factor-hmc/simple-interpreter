module Lang exposing (..)

import List
import String

type Literal
  = Int Int
  | Float Float
  | T
  | F
  | String String
  | Array (List Literal)
  | Quotation (List Word)

type Builtin
  = Push Literal
  | Add
  | Sub
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

type Word
    = Word String
    | Builtin Builtin

type alias Stack = List Literal

toBool : Literal -> Bool
toBool lit =
    case lit of
        F -> False
        _ -> True
