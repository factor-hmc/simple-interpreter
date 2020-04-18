module Factor.Parser exposing (..)

import Char
import Dict
import Factor.Lang exposing (..)
import Factor.Show as Show
import Parser exposing (..)
import Parser.Extras exposing (..)
import Set
import Util


spaces : Parser ()
spaces =
    chompWhile Util.isWhitespace
        |. oneOf
            [ symbol "!"
                |. chompIf (\c -> Util.isWhitespace c && c /= '\n')
                |. chompUntilEndOr "\n"
                |. chompWhile Util.isWhitespace
                |> many
                |> map (always ())
            , succeed ()
            ]


someSpaces : Parser ()
someSpaces =
    chompIf Util.isWhitespace |. spaces


spacesBefore : List a -> Parser ()
spacesBefore l =
    case l of
        [] ->
            succeed ()

        _ ->
            someSpaces


string : Parser String
string =
    let
        special =
            Set.fromList
                [ '"'
                , '\\'
                ]

        escapes =
            [ ( '"', "\"" )
            , ( '\\', "\\" )
            , ( 'n', "\n" )
            , ( 't', "\t" )
            ]

        escaped =
            succeed identity
                |. symbol "\\"
                |= oneOf
                    (escapes
                        |> List.map
                            (\( c, s ) -> succeed s |. symbol (String.fromChar c))
                    )
    in
    succeed identity
        |. symbol "\""
        |= loop
            []
            (\s ->
                oneOf
                    [ oneOf
                        [ chompIf (\c -> not <| Set.member c special)
                            |> getChompedString
                        , escaped
                        ]
                        |> map (\c -> Loop (c :: s))
                    , symbol "\""
                        |> map (\() -> Done (s |> List.reverse |> String.concat))
                    ]
            )


token : Parser Token
token =
    oneOf
        [ map (Literal << String) string
        , variable
            { start = not << Util.isWhitespace
            , inner = not << Util.isWhitespace
            , reserved = Set.empty
            }
            |> getChompedString
            |> map
                (\s ->
                    run
                        (number
                            { int = Just Int
                            , hex = Just Int
                            , octal = Just Int
                            , binary = Just Int
                            , float = Just Float
                            }
                            |. end
                        )
                        s
                        |> Result.map Literal
                        |> Result.withDefault (Word s)
                )
        ]


quotation : Parser Action
quotation =
    succeed (Builtin << Push << Quotation)
        |. someSpaces
        |= actions (Just <| Word "]")


array : Parser Action
array =
    succeed (Builtin << Push << Array)
        |. someSpaces
        |= actions (Just <| Word "}")


definition : Parser Action
definition =
    succeed Definition
        |. someSpaces
        |= (token
                |> andThen
                    (\t ->
                        case t of
                            Word s ->
                                succeed s

                            Literal l ->
                                problem <| "invalid word name " ++ Show.literal l
                    )
           )
        |. someSpaces
        |= effect_
        |. someSpaces
        |= actions (Just <| Word ";")
        |. oneOf [ backtrackable (someSpaces |. keyword "inline"), succeed () ]


effect_ : Parser Effect
effect_ =
    let
        idents res end =
            loop [] <|
                \is ->
                    succeed identity
                        |. spacesBefore is
                        |= variable
                            { start = not << Util.isWhitespace
                            , inner = not << Util.isWhitespace
                            , reserved = Set.fromList [ res ]
                            }
                        |> andThen
                            (\i ->
                                if i == res then
                                    problem <| "reserved " ++ res

                                else if i == end then
                                    succeed <| Done <| List.reverse is

                                else
                                    succeed <| Loop <| i :: is
                            )
    in
    succeed Effect
        |. symbol "("
        |. someSpaces
        |= idents ")" "--"
        |. someSpaces
        |= idents "--" ")"


action : Token -> Parser Action
action t =
    case t of
        Word "[" ->
            quotation

        Word "{" ->
            array

        Word ":" ->
            definition

        Word w ->
            succeed <| Apply w

        Literal l ->
            succeed <| Builtin <| Push l


actions : Maybe Token -> Parser (List Action)
actions end =
    loop [] <|
        let
            nextToken acts f =
                succeed identity
                    |. spacesBefore acts
                    |= token
                    |> andThen f

            done =
                succeed << Done << List.reverse

            addAction acts t =
                action t |> map (\a -> Loop <| a :: acts)
        in
        case end of
            Nothing ->
                \acts ->
                    oneOf
                        [ backtrackable <| nextToken acts <| addAction acts
                        , done acts
                        ]

            Just e ->
                \acts ->
                    nextToken acts
                        (\t ->
                            if t == e then
                                done acts

                            else
                                addAction acts t
                        )


input : Parser (List Action)
input =
    succeed identity
        |. spaces
        |= actions Nothing
        |. spaces
        |. end
