module Eval exposing (..)

import Lang exposing (..)
import List exposing (foldl)
import Result

numericBinaryOp : (Float -> Float -> Float) -> (Int -> Int -> Int) -> Stack -> Result String Stack
numericBinaryOp floatOp intOp stack =
    case stack of
        (Int x::Int y::rest)     -> Ok <| Int (intOp y x) :: rest
        (Float x::Float y::rest) -> Ok <| Float (floatOp y x) :: rest
        (Int x::Float y::rest) -> Ok <| Float (floatOp y (toFloat x)) :: rest
        (Float x::Int y::rest) -> Ok <| Float (floatOp (toFloat y) x) :: rest
        [x] -> Err "Two numeric arguments expected, got only one element on stack."
        [] -> Err "Two numeric arguments expected, got empty stack."
        _  -> Err "Two numeric arguments expected, got non-numeric type."

-- yes indeed we have to special-case division because it can produce floats.
divide : Stack -> Result String Stack
divide stack =
    case stack of
        (Int x::Int y::rest) -> Ok <| Float (toFloat y / toFloat x) :: rest
        (Float x::Float y::rest) -> Ok <| Float (y / x) :: rest
        (Int x::Float y::rest) -> Ok <| Float (y/ (toFloat x)) :: rest
        (Float x::Int y::rest) -> Ok <| Float ((toFloat y) / x) :: rest
        [x] -> Err "Two numeric arguments expected, got only one element on stack."
        [] -> Err "Two numeric arguments expected, got empty stack."
        _  -> Err "Two numeric arguments expected, got non-numeric type."

evalBuiltin : Stack -> Builtin -> Result String Stack
evalBuiltin stack builtin =
    case (stack, builtin) of
        (_, Add) -> numericBinaryOp (+) (+) stack
        (_, Sub) -> numericBinaryOp (-) (-) stack
        (_, Mul) -> numericBinaryOp (*) (*) stack
        (_, Div) -> divide stack
        (x::y::rest, Eq) -> Ok <|
            if (x == y)
                then T :: rest
                else F :: rest
        (Quotation whenFalse::Quotation whenTrue::b::rest, If) ->
            if toBool b
                then evalWords rest whenTrue
                else evalWords rest whenFalse
        (x::rest, Dup) -> Ok <| x::x::rest
        (x::y::rest, Swap) -> Ok <| y::x::rest
        (x::y::z::rest, Rot) -> Ok <| z::x::y::rest
        (x::rest, Drop) -> Ok <| rest
        (_, Push lit) -> Ok <| lit::stack
        (Quotation body::Quotation pred::rest, While) -> evalWhile rest body pred
        (_, Clear) -> Ok []
        _ -> Err "Invalid stack contents."

eval : Stack -> Word -> Result String Stack
eval stack w =
    case w of
        Builtin b -> evalBuiltin stack b
        Word word -> Err "case Word: Unimplemented"

evalWords : Stack -> List Word -> Result String Stack
evalWords stack = foldl (\w -> Result.andThen (\st -> eval st w)) (Ok stack)

evalWhile : Stack -> List Word -> List Word -> Result String Stack
evalWhile stack body pred =
    case evalWords stack pred of
        Ok (b::rest) ->
            if toBool b
                then case evalWords rest body of
                         Ok newStack -> evalWhile newStack body pred
                         Err e       -> Err e
                else Ok rest
        Ok [] -> Err "while: empty stack."
        Err e -> Err e
