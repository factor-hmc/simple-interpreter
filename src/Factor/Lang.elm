module Factor.Lang exposing (..)

import List
import String


type Token
    = Literal Literal
    | Word String


type Literal
    = Int Int
    | Float Float
    | T
    | F
    | String String
    | Array (List Action)
    | Quotation (List Action)


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
    | Nth


type alias Effect =
    { ins : List String
    , outs : List String
    }


type Action
    = Apply String
    | Definition String Effect (List Action)
    | Builtin Builtin


type alias Stack =
    List Literal
