module Factor.Runtime exposing (..)

import Dict exposing (Dict)
import Factor.Lang exposing (..)
import Factor.Parser
import Factor.Vocabs as Vocabs
import List exposing (foldl)
import Parser exposing ((|.))
import Result


type alias Lookup =
    Dict String (List Action)


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


toFactorBool : Bool -> Literal
toFactorBool b =
    if b then
        T

    else
        F


primitiveLookup : Lookup
primitiveLookup =
    [ ( "drop", Drop )
    , ( "swap", Swap )
    , ( "+", Add )
    , ( "-", Sub )
    , ( "*", Mul )
    , ( "/", Div )
    , ( "=", Eq )
    , ( "<", Lt )
    , ( "<=", Le )
    , ( ">", Gt )
    , ( ">=", Ge )
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
    , ( "t", Push T )
    , ( "f", Push F )
    ]
        |> List.map (Tuple.mapSecond (Builtin >> List.singleton))
        |> Dict.fromList


type BuiltinDef
    = Quot (List BuiltinDef)
    | Pre String


compileDSL : Lookup -> BuiltinDef -> List Action
compileDSL lookup def =
    case def of
        Pre s ->
            Dict.get s lookup |> Maybe.withDefault []

        Quot defs ->
            List.concatMap (compileDSL lookup) defs
                |> (Quotation >> Push >> Builtin >> List.singleton)


run : String -> State -> Result String State
run code state =
    Parser.run Factor.Parser.input code
        |> Result.mapError (always "fail to parse")
        |> Result.andThen (eval { state | output = [] })


default : State
default =
    { stack = []
    , lookup = primitiveLookup
    , output = []
    }


init : State
init =
    default
        |> run (Dict.get "kernel" Vocabs.vocabs |> Maybe.withDefault "")
        |> Result.andThen (run (Dict.get "math" Vocabs.vocabs |> Maybe.withDefault ""))
        |> Result.withDefault default


numericOp :
    (Float -> Float -> Literal)
    -> (Int -> Int -> Literal)
    -> Stack
    -> Result String Stack
numericOp f i s =
    case s of
        (Int y) :: (Int x) :: rest ->
            Ok <| i x y :: rest

        (Int y) :: (Float x) :: rest ->
            Ok <| f x (toFloat y) :: rest

        (Float y) :: (Int x) :: rest ->
            Ok <| f (toFloat x) y :: rest

        (Float y) :: (Float x) :: rest ->
            Ok <| f x y :: rest

        _ ->
            Err "invalid numbers"


evalBuiltin : State -> Builtin -> Result String State
evalBuiltin state builtin =
    let
        setStack s =
            { state | stack = s }

        setStackAndPrint s p =
            { state | stack = s, output = p :: state.output }

        stack =
            state.stack

        bcompose =
            (<<) << (<<)

        arith f i =
            numericOp (bcompose Float f) (bcompose Int i) stack
                |> Result.map setStack

        comp f i =
            let
                wrap =
                    bcompose toFactorBool
            in
            numericOp (wrap f) (wrap i) stack
                |> Result.map setStack

        okStack =
            setStack >> Ok
    in
    case ( stack, builtin ) of
        ( _, Add ) ->
            arith (+) (+)

        ( _, Sub ) ->
            arith (-) (-)

        ( _, Mul ) ->
            arith (*) (*)

        ( _, Div ) ->
            numericOp
                (bcompose Float (/))
                (\a b -> Float <| toFloat a / toFloat b)
                stack
                |> Result.map setStack

        ( x :: y :: rest, Eq ) ->
            okStack <| toFactorBool (x == y) :: rest

        ( _, Lt ) ->
            comp (<) (<)

        ( _, Le ) ->
            comp (<=) (<=)

        ( _, Gt ) ->
            comp (>) (>)

        ( _, Ge ) ->
            comp (>=) (>=)

        ( (Quotation whenFalse) :: (Quotation whenTrue) :: b :: rest, If ) ->
            if toBool b then
                eval (setStack rest) whenTrue

            else
                eval (setStack rest) whenFalse

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
            eval (setStack rest) q

        ( (Quotation q) :: top :: rest, Dip ) ->
            eval (setStack rest) q
                |> Result.map (\st -> { st | stack = top :: st.stack })

        ( (Quotation q) :: (Quotation r) :: rest, Compose ) ->
            okStack <| Quotation (r ++ q) :: rest

        ( (Quotation q) :: x :: rest, Curry ) ->
            okStack <| Quotation (Builtin (Push x) :: q) :: rest

        ( (Array a) :: (Int i) :: rest, nth ) ->
            okStack rest

        _ ->
            Err "Invalid stack contents."


evalOne : State -> Action -> Result String State
evalOne state w =
    case w of
        Builtin b ->
            evalBuiltin state b

        Apply word ->
            Dict.get word state.lookup
                |> Maybe.map (eval state)
                |> Maybe.withDefault (Err <| "Unimplemented word " ++ word)

        Definition name _ body ->
            Ok { state | lookup = Dict.insert name body state.lookup }


eval : State -> List Action -> Result String State
eval state =
    foldl (\w -> Result.andThen (\st -> evalOne st w)) (Ok state)


evalWhile : State -> List Action -> List Action -> Result String State
evalWhile state body pred =
    eval state pred
        |> Result.andThen
            (\st ->
                case st.stack of
                    b :: rest ->
                        if toBool b then
                            eval { st | stack = rest } body
                                |> Result.andThen
                                    (\newStack -> evalWhile newStack body pred)

                        else
                            Ok { st | stack = rest }

                    [] ->
                        Err "while: empty stack."
            )
