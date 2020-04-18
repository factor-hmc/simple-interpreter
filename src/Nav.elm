module Nav exposing (..)

import Http
import Url
import Url.Builder
import Url.Parser as UP exposing ((</>))
import Browser.Navigation as BNav

type alias Msg =
    Url.Url


type Dest
    = Book (List String)
    | Foogle

type alias Model =
    { app : Dest
    }

init : Model
init =
    { app = Book []
    }


bookParser : UP.Parser (List String -> a) a
bookParser =
    UP.oneOf
        [ UP.map (\a -> [ a ]) UP.string
        , UP.map (\a b -> [ a, b ]) <| UP.string </> UP.string
        , UP.map (\a b c -> [ a, b, c ]) <| UP.string </> UP.string </> UP.string
        ]


parser : UP.Parser (Dest -> a) a
parser =
    UP.oneOf
        [ UP.s "book" </> UP.map Book bookParser
        , UP.s "foogle" |> UP.map Foogle
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


replaceExt : String -> String -> String -> String
replaceExt a b =
    String.split "."
        >> mapLast
            (\ext ->
                if ext == a then
                    b

                else
                    ext
            )
        >> String.join "."


bookPathToJson : List String -> List String
bookPathToJson =
    mapLast <| replaceExt "md" "json"


update : (Result Http.Error String -> loadMsg) -> BNav.Key -> Msg -> Model -> (Model, Cmd loadMsg)
update load key u model =
    case UP.parse parser u of
        Just (Book path) ->
            ({app = Book path}, Cmd.batch
                [ Http.get
                    { expect = Http.expectString load
                    , url =
                        Url.Builder.crossOrigin
                            "https://factor-book.netlify.app"
                            ("json" :: bookPathToJson path)
                            []
                    }
                , BNav.pushUrl key <| Url.toString u
                ]
                 )

        Just Foogle ->
            ({app = Foogle}, BNav.pushUrl key <| Url.toString u)

        Nothing ->
            (model, Cmd.none)
