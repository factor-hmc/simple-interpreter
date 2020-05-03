module Book.Url exposing (..)

import Url.Parser exposing (..)


parser : Parser (List String -> a) a
parser =
    oneOf
        [ map (\a -> [ a ]) string
        , map (\a b -> [ a, b ]) <| string </> string
        , map (\a b c -> [ a, b, c ]) <| string </> string </> string
        , map (\a b c d -> [ a, b, c, d ]) <|
            string
                </> string
                </> string
                </> string
        , map (\a b c d e -> [ a, b, c, d, e ]) <|
            string
                </> string
                </> string
                </> string
                </> string
        ]


mapHead : (a -> a) -> List a -> List a
mapHead f l =
    case l of
        a :: rest ->
            f a :: rest

        _ ->
            l


mapLast : (a -> a) -> List a -> List a
mapLast f =
    List.reverse >> mapHead f >> List.reverse


setExt : String -> String -> String -> String
setExt a b =
    String.split "."
        >> mapLast
            (\ext ->
                if ext == a then
                    b

                else
                    ext
            )
        >> String.join "."

replaceExt : String -> String -> List String -> List String
replaceExt =
    ((<<) << (<<)) mapLast setExt

pathToJson : List String -> List String
pathToJson =
    replaceExt "md" "json" >> replaceExt "html" "json"
