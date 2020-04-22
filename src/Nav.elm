module Nav exposing (..)

import Book
import Browser
import Browser.Navigation
import Http
import Json.Decode
import Url
import Url.Builder
import Url.Parser exposing (..)


type Load
    = Load (Result Http.Error Book.Book)


type Msg
    = Url Browser.UrlRequest


type Dest
    = Book (List String)
    | Foogle


type alias Model =
    { app : Dest
    , key : Browser.Navigation.Key
    }


init : Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Load )
init url key =
    case parse parser url of
        Just (Book path) ->
            ( { app = Book path
              , key = key
              }
            , loadBookPath path
            )

        Just Foogle ->
            ( { app = Foogle
              , key = key
              }
            , loadBookPath [ "README.md" ]
            )

        Nothing ->
            ( { app = Book []
              , key = key
              }
            , Browser.Navigation.load "/book/README.md"
            )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    Url


bookParser : Url.Parser.Parser (List String -> a) a
bookParser =
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


parser : Parser (Dest -> a) a
parser =
    oneOf
        [ s "book" </> map Book bookParser
        , s "foogle" |> map Foogle
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


loadPath : List String -> Cmd Load
loadPath path =
    Http.get
        { expect = Http.expectJson Load Book.book
        , url =
            Url.Builder.crossOrigin
                "https://factor-book.netlify.app"
                ("json" :: path)
                []
        }


loadBookPath : List String -> Cmd Load
loadBookPath =
    loadPath << bookPathToJson


update : Msg -> Model -> ( Model, Cmd Load )
update msg mod =
    case msg of
        Url (Browser.External _) ->
            ( mod, Cmd.none )

        Url (Browser.Internal url) ->
            case parse parser url of
                Just (Book path) ->
                    ( { mod | app = Book path }
                    , Cmd.batch
                        [ loadPath <| bookPathToJson path
                        , Browser.Navigation.pushUrl mod.key <| Url.toString url
                        ]
                    )

                Just Foogle ->
                    ( { mod | app = Foogle }
                    , Browser.Navigation.pushUrl mod.key <| Url.toString url
                    )

                Nothing ->
                    ( mod, Cmd.none )
