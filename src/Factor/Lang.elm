module Factor.Lang exposing (..)

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
  | Print
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
  | Call
  | Dip
  | Compose
  | Curry

type alias Effect =
    { ins : List String
    , outs : List String
    }

type Word
    = Word String
    | Definition String Effect (List Word)
    | Builtin Builtin

type alias Stack = List Literal
