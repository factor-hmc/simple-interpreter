module Factor.Runtime exposing (..)

import Dict exposing (Dict)
import Factor.Lang exposing (..)
import Factor.Parser
import List exposing (foldl)
import Parser
import Result


type alias Lookup =
    Dict String (List Word)


type alias State =
    { stack : Stack
    , lookup : Lookup
    , output : List String
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


builtins : String
builtins =
    [ ( "over", "[ dup ] dip swap" )
    , ( "pick", "[ over ] dip swap" )
    , ( "reach", "[ pick ] dip swap" )
    , ( "-rot", "rot rot" )
    , ( "keep", "over [ call ] dip" )
    , ( "nip", "[ drop ] dip" )
    , ( "bi", "[ keep ] dip call" )
    , ( "bi*", "[ dip ] dip call" )
    , ( "bi@", "dup bi*" )
    , ( "tuck", "dup -rot" )
    , ( "unless", "swap [ drop ] [ call ] if" )
    , ( "when", "swap [ call ] [ drop ] if" )
    , ( "tri", "[ [ keep ] dip keep ] dip call" )
    ]
        ++ ([ "nip", "dup", "swap", "over", "pick", "rot" ]
                |> List.map (\name -> ( name ++ "d", "[ " ++ name ++ " ] dip" ))
           )
        ++ [ ( "roll", "rotd swap" )
           ]
        |> List.map (\( name, def ) -> ": " ++ name ++ " ( -- ) " ++ def ++ " ;")
        |> String.join "\n"


run : String -> State -> Result String State
run code state =
    Parser.run Factor.Parser.words code
        |> Result.mapError (always "parse error")
        |> Result.andThen (evalWords { state | output = [] })
{-

initLookup : Lookup
initLookup =
    [ ( "2drop", List.repeat 2 <| Pre "drop" )
    , ( "3drop", List.repeat 3 <| Pre "drop" )
    , ( "4drop", List.repeat 4 <| Pre "drop" )
    , ( "2dup", List.repeat 2 <| Pre "over" )
    , ( "3dup", List.repeat 3 <| Pre "pick" )
    , ( "2nip", [ Quot [ Pre "2drop" ], Pre "dip" ] )
    , ( "tri*", [ Quot [ Quot [ Pre "2dip" ], Pre "dip", Pre "dip" ], Pre "dip", Pre "call" ] )
    , ( "tri@", [ Pre "dup", Pre "dup", Pre "tri*" ] )
    , ( "2dip", [ Pre "swap", Quot [ Pre "dip" ], Pre "dip" ] )
    ]
        |> List.foldl
            (\( name, defs ) lookup ->
                Dict.insert name (List.concatMap (compileDSL lookup) defs) lookup
            )
            primitiveLookup
-}

default : State
default =
    { stack = []
    , lookup = primitiveLookup
    , output = []
    }


init : State
init =
    --let
    --    _ =
    --        Debug.log "run" (default |> run builtins)
    --in
    default |> run builtins |> Result.withDefault default


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
            { state | stack = s, output = [] }

        setStackAndPrint s p =
            { state | stack = s, output = p :: state.output }

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
