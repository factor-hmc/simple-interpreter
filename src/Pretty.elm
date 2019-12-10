module Pretty exposing (..)

import Lang exposing (..)

showLiteral : Literal -> String
showLiteral lit =
    case lit of
        Int i           -> String.fromInt i
        Float f         ->
            if floor f == ceiling f
            then String.fromInt (truncate f) ++ ".0"
            else String.fromFloat f
        T               -> "t"
        F               -> "f"
        String str      -> "\"" ++ str ++ "\""
        Array arr       -> "{ " ++ String.join " " (List.map showLiteral arr) ++ " }"
        Quotation words -> "[ " ++ String.join " " (List.map showWord words) ++ " ]"

showBuiltin : Builtin -> String
showBuiltin b =
    case b of
        Push lit  -> showLiteral lit
        Add   -> "+"
        Sub   -> "-"
        Mul   -> "*"
        Div   -> "/"
        Eq    -> "="
        If    -> "if"
        Dup   -> "dup"
        Swap  -> "swap"
        Rot   -> "rot"
        Drop  -> "drop"
        While -> "while"
        Clear -> "clear"

showWord : Word -> String
showWord word =
    case word of
        Word str  -> str
        Builtin b -> showBuiltin b

