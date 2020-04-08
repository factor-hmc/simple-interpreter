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


toBool : Literal -> Bool
toBool lit =
    case lit of
        F ->
            False

        _ ->
            True


primitiveLookup : Lookup
primitiveLookup =
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
    , ( "call", Call )
    , ( "dip", Dip )
    , ( "compose", Compose )
    , ( "curry", Curry )
    ]
        |> List.map (Tuple.mapSecond (Builtin >> List.singleton))
        |> Dict.fromList


type BuiltinDef
    = Quot (List BuiltinDef)
    | Pre String


compileDSL : Lookup -> BuiltinDef -> List Word
compileDSL lookup def =
    case def of
        Pre s ->
            Dict.get s lookup |> Maybe.withDefault []

        Quot defs ->
            List.concatMap (compileDSL lookup) defs
                |> (Quotation >> Push >> Builtin >> List.singleton)


initLookup : Lookup
initLookup =
    [ ( "keep", [ Pre "over", Quot [ Pre "call" ], Pre "dip" ] )
    , ( "-rot", [ Pre "rot", Pre "rot" ] )
    , ( "over", [ Quot [ Pre "dup" ], Pre "dip", Pre "swap" ] )
    , ( "pick", [ Quot [ Pre "over" ], Pre "dip", Pre "swap" ] )
    , ( "reach", [ Quot [ Pre "pick" ], Pre "dip", Pre "swap" ] )
    , ( "2drop", List.repeat 2 <| Pre "drop" )
    , ( "3drop", List.repeat 3 <| Pre "drop" )
    , ( "4drop", List.repeat 4 <| Pre "drop" )
    , ( "2dup", List.repeat 2 <| Pre "over" )
    , ( "3dup", List.repeat 3 <| Pre "pick" )
    , ( "nip", [ Quot [ Pre "drop" ], Pre "dip" ] )
    , ( "nipd", [ Quot [ Pre "nip" ], Pre "dip" ] )
    , ( "2nip", [ Quot [ Pre "2drop" ], Pre "dip" ] )
    , ( "dupd", [ Quot [ Pre "dup" ], Pre "dip" ] )
    , ( "swapd", [ Quot [ Pre "swap" ], Pre "dip" ] )
    , ( "bi", [ Quot [ Pre "keep" ], Pre "dip", Pre "call" ] )
    , ( "bi*", [ Quot [ Pre "dip" ], Pre "dip", Pre "call" ] )
    , ( "bi@", [ Pre "dup", Pre "bi*" ] )
    , ( "overd", [ Quot [ Pre "over" ], Pre "dip" ] )
    , ( "pickd", [ Quot [ Pre "pick" ], Pre "dip" ] )
    , ( "rotd", [ Quot [ Pre "rot" ], Pre "dip" ] )
    , ( "roll", [ Pre "rotd", Pre "swap" ] )
    , ( "tri", [ Quot [ Quot [ Pre "keep" ], Pre "dip", Pre "keep" ], Pre "dip", Pre "call" ] )
    , ( "tri*", [ Quot [ Quot [ Pre "2dip" ], Pre "dip", Pre "dip" ], Pre "dip", Pre "call" ] )
    , ( "tri@", [ Pre "dup", Pre "dup", Pre "tri*" ] )
    , ( "tuck", [ Pre "dup", Pre "-rot" ] )
    , ( "unless", [ Pre "swap", Quot [ Pre "drop" ], Quot [ Pre "call" ], Pre "if" ] )
    , ( "2dip", [ Pre "swap", Quot [ Pre "dip" ], Pre "dip" ] )
    , ( "do", [ Pre "dup", Pre "2dip" ] )
    , ( "when", [ Pre "swap", Quot [ Pre "call" ], Quot [ Pre "drop" ], Pre "if" ] )
    ]
        |> List.foldl
            (\( name, defs ) lookup ->
                Dict.insert name (List.concatMap (compileDSL lookup) defs) lookup
            )
            primitiveLookup


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

        ( (Quotation q) :: rest, Call ) ->
            evalWords (setStack rest) q

        ( (Quotation q) :: top :: rest, Dip ) ->
            evalWords (setStack rest) q
                |> Result.map (\st -> { st | stack = top :: st.stack })

        ( (Quotation q) :: (Quotation r) :: rest, Compose ) ->
            okStack <| Quotation (r ++ q) :: rest

        ( (Quotation q) :: x :: rest, Curry ) ->
            okStack <| Quotation (Builtin (Push x) :: q) :: rest

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
