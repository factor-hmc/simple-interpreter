module Factor.Show exposing (..)

import Factor.Lang exposing (..)


escape : Char -> String
escape c =
    case c of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\n' ->
            "\\n"

        '\t' ->
            "\\t"

        _ ->
            String.fromChar c


string : String -> String
string s =
    "\"" ++ (String.toList s |> List.map escape |> String.concat) ++ "\""


literal : Literal -> String
literal lit =
    case lit of
        Int i ->
            String.fromInt i

        Float f ->
            if floor f == ceiling f then
                String.fromInt (truncate f) ++ ".0"

            else
                String.fromFloat f

        T ->
            "t"

        F ->
            "f"

        String s ->
            string s

        Array arr ->
            "{ " ++ String.join " " (List.map action arr) ++ " }"

        Quotation acts ->
            "[ " ++ String.join " " (List.map action acts) ++ " ]"


builtin : Builtin -> String
builtin b =
    case b of
        Push lit ->
            literal lit

        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"

        Eq ->
            "="

        Lt ->
            "<"

        Le ->
            "<="

        Gt ->
            ">"

        Ge ->
            ">="

        If ->
            "if"

        Dup ->
            "dup"

        Swap ->
            "swap"

        Rot ->
            "rot"

        Drop ->
            "drop"

        While ->
            "while"

        Print ->
            "print"

        Call ->
            "call"

        Clear ->
            "clear"

        Dip ->
            "dip"

        Compose ->
            "compose"

        Curry ->
            "curry"

        Nth ->
            "nth"


effect_ : Effect -> String
effect_ eff =
    "( "
        ++ String.join " " eff.ins
        ++ " -- "
        ++ String.join " " eff.outs
        ++ " )"


action : Action -> String
action w =
    case w of
        Apply str ->
            str

        Builtin b ->
            builtin b

        Definition name eff body ->
            ": "
                ++ name
                ++ " "
                ++ effect_ eff
                ++ " "
                ++ (List.map action body |> String.join " ")
