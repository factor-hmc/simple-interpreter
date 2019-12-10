module Eval exposing (..)

import Dict exposing (Dict)
import Lang exposing (..)
import List exposing (foldl)
import Result


type alias Lookup =
    Dict String (List Word)


type alias State =
    { stack : Stack
    , lookup : Lookup
    }


initLookup : Lookup
initLookup =
    [ ( "drop", Drop )
    , ( "swap", Swap )
    , ( "+", Add )
    , ( "-", Sub )
    , ( "*", Mul )
    , ( "/", Div )
    , ( "eq", Eq )
    , ( "if", If )
    , ( "dup", Dup )
    , ( "swap", Swap )
    , ( "rot", Rot )
    , ( "drop", Drop )
    , ( "while", While )
    , ( "clear", Clear )
    ]
        |> List.map (Tuple.mapSecond (Builtin >> List.singleton))
        |> Dict.fromList


init : State
init =
    { stack = []
    , lookup = initLookup
    }


numericBinaryOp : (Float -> Float -> Float) -> (Int -> Int -> Int) -> Stack -> Result String Stack
numericBinaryOp floatOp intOp stack =
    case stack of
        (Int x) :: (Int y) :: rest ->
            Ok <| Int (intOp y x) :: rest

        (Float x) :: (Float y) :: rest ->
            Ok <| Float (floatOp y x) :: rest

        (Int x) :: (Float y) :: rest ->
            Ok <| Float (floatOp y (toFloat x)) :: rest

        (Float x) :: (Int y) :: rest ->
            Ok <| Float (floatOp (toFloat y) x) :: rest

        [ x ] ->
            Err "Two numeric arguments expected, got only one element on stack."

        [] ->
            Err "Two numeric arguments expected, got empty stack."

        _ ->
            Err "Two numeric arguments expected, got non-numeric type."



-- yes indeed we have to special-case division because it can produce floats.


divide : Stack -> Result String Stack
divide stack =
    case stack of
        (Int x) :: (Int y) :: rest ->
            Ok <| Float (toFloat y / toFloat x) :: rest

        (Float x) :: (Float y) :: rest ->
            Ok <| Float (y / x) :: rest

        (Int x) :: (Float y) :: rest ->
            Ok <| Float (y / toFloat x) :: rest

        (Float x) :: (Int y) :: rest ->
            Ok <| Float (toFloat y / x) :: rest

        [ x ] ->
            Err "Two numeric arguments expected, got only one element on stack."

        [] ->
            Err "Two numeric arguments expected, got empty stack."

        _ ->
            Err "Two numeric arguments expected, got non-numeric type."


evalBuiltin : State -> Builtin -> Result String State
evalBuiltin state builtin =
    let
        setStackIn st s =
            { st | stack = s }

        stack =
            state.stack

        op a b =
            numericBinaryOp a b stack
                |> Result.map (setStackIn state)

        okStack =
            setStackIn state >> Ok
    in
    case ( stack, builtin ) of
        ( _, Add ) ->
            op (+) (+)

        ( _, Sub ) ->
            op (-) (-)

        ( _, Mul ) ->
            op (*) (*)

        ( _, Div ) ->
            divide stack |> Result.map (setStackIn state)

        ( x :: y :: rest, Eq ) ->
            (if x == y then
                T :: rest

             else
                F :: rest
            )
                |> okStack

        ( (Quotation whenFalse) :: (Quotation whenTrue) :: b :: rest, If ) ->
            if toBool b then
                evalWords (setStackIn state rest) whenTrue

            else
                evalWords (setStackIn state rest) whenFalse

        ( x :: rest, Dup ) ->
            okStack <| x :: x :: rest

        ( x :: y :: rest, Swap ) ->
            okStack <| y :: x :: rest

        ( x :: y :: z :: rest, Rot ) ->
            okStack <| z :: x :: y :: rest

        ( x :: rest, Drop ) ->
            okStack <| rest

        ( _, Push lit ) ->
            okStack <| lit :: stack

        ( (Quotation body) :: (Quotation pred) :: rest, While ) ->
            evalWhile { state | stack = rest } body pred

        ( _, Clear ) ->
            okStack []

        _ ->
            Err "Invalid stack contents."


eval : State -> Word -> Result String State
eval state w =
    case w of
        Builtin b ->
            evalBuiltin state b

        Word word ->
            Dict.get word state.lookup
                |> Maybe.map (evalWords state)
                |> Maybe.withDefault (Err <| "Unimplemented word " ++ word)


evalWords : State -> List Word -> Result String State
evalWords state =
    foldl (\w -> Result.andThen (\st -> eval st w)) (Ok state)


evalWhile : State -> List Word -> List Word -> Result String State
evalWhile state body pred =
    let
        predState =
            evalWords state pred
    in
    case predState of
        Ok st ->
            case st.stack of
                b :: rest ->
                    if toBool b then
                        case evalWords { st | stack = rest } body of
                            Ok newStack ->
                                evalWhile newStack body pred

                            Err e ->
                                Err e

                    else
                        Ok { st | stack = rest }

                [] ->
                    Err "while: empty stack."

        Err e ->
            Err e
