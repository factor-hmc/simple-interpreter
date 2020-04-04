module Factor.Runtime exposing (..)

import Dict exposing (Dict)
import Factor.Lang exposing (..)
import List exposing (foldl)
import Result


type alias Lookup =
    Dict String (List Word)


type alias State =
    { stack : Stack
    , lookup : Lookup
    , output : Maybe String
    }


initLookup : Lookup
initLookup =
    [ ( "drop", Drop )
    , ( "swap", Swap )
    , ( "+", Add )
    , ( "-", Sub )
    , ( "*", Mul )
    , ( "/", Div )
    , ( "=", Eq )
    , ( "if", If )
    , ( "dup", Dup )
    , ( "swap", Swap )
    , ( "rot", Rot )
    , ( "drop", Drop )
    , ( "while", While )
    , ( "clear", Clear )
    , ( "print", Print )
    ]
        |> List.map (Tuple.mapSecond (Builtin >> List.singleton))
        |> Dict.fromList


init : State
init =
    { stack = []
    , lookup = initLookup
    , output = Nothing
    }


numericBinaryOp :
    (Float -> Float -> Float)
    -> (Int -> Int -> Int)
    -> Stack
    -> Result String Stack
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
        setStack s =
            { state | stack = s, output = Nothing }

        setStackAndPrint s p =
            { state | stack = s, output = Just p }

        stack =
            state.stack

        op a b =
            numericBinaryOp a b stack
                |> Result.map setStack

        okStack =
            setStack >> Ok
    in
    case ( stack, builtin ) of
        ( _, Add ) ->
            op (+) (+)

        ( _, Sub ) ->
            op (-) (-)

        ( _, Mul ) ->
            op (*) (*)

        ( _, Div ) ->
            divide stack |> Result.map setStack

        ( x :: y :: rest, Eq ) ->
            (if x == y then
                T :: rest

             else
                F :: rest
            )
                |> okStack

        ( (Quotation whenFalse) :: (Quotation whenTrue) :: b :: rest, If ) ->
            if toBool b then
                evalWords (setStack rest) whenTrue

            else
                evalWords (setStack rest) whenFalse

        ( x :: rest, Dup ) ->
            okStack <| x :: x :: rest

        ( x :: y :: rest, Swap ) ->
            okStack <| y :: x :: rest

        ( x :: y :: z :: rest, Rot ) ->
            okStack <| z :: x :: y :: rest

        ( x :: rest, Drop ) ->
            okStack <| rest

        ( x :: rest, Print ) ->
            case x of
                String s ->
                    Ok <| setStackAndPrint rest s

                _ ->
                    Err "tried to print non-string"

        ( _, Push lit ) ->
            okStack <| lit :: stack

        ( (Quotation body) :: (Quotation pred) :: rest, While ) ->
            evalWhile (setStack rest) body pred

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

        Definition name _ body ->
            Ok { state | lookup = Dict.insert name body state.lookup }


evalWords : State -> List Word -> Result String State
evalWords state =
    foldl (\w -> Result.andThen (\st -> eval st w)) (Ok state)


evalWhile : State -> List Word -> List Word -> Result String State
evalWhile state body pred =
    evalWords state pred
        |> Result.andThen
            (\st ->
                case st.stack of
                    b :: rest ->
                        if toBool b then
                            evalWords { st | stack = rest } body
                                |> Result.andThen
                                    (\newStack -> evalWhile newStack body pred)

                        else
                            Ok { st | stack = rest }

                    [] ->
                        Err "while: empty stack."
            )
