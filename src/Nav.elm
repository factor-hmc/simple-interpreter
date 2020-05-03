module Nav exposing (..)

import Book
import Book.Url
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
    , url : Url.Url
    }


init : Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Load )
init url key =
    case parse parser url of
        Just (Book path) ->
            ( { app = Book path
              , key = key
              , url = url
              }
            , loadBookPath path
            )

        Just Foogle ->
            ( { app = Foogle
              , key = key
              , url = url
              }
            , loadBookPath [ "README.md" ]
            )

        Nothing ->
            ( { app = Book []
              , key = key
              , url = url
              }
            , Browser.Navigation.load "/book/README.md"
            )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    Url


parser : Parser (Dest -> a) a
parser =
    oneOf
        [ s "book" </> map Book Book.Url.parser
        , s "foogle" |> map Foogle
        ]


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
    loadPath << Book.Url.pathToJson


update : Msg -> Model -> ( Model, Cmd Load )
update msg mod =
    case msg of
        Url (Browser.External _) ->
            ( mod, Cmd.none )

        Url (Browser.Internal url) ->
            case parse parser url of
                Just (Book path) ->
                    ( { mod | app = Book path, url = url }
                    , Cmd.batch
                        [ loadBookPath path
                        , Browser.Navigation.pushUrl mod.key <| Url.toString url
                        ]
                    )

                Just Foogle ->
                    ( { mod | app = Foogle, url = url }
                    , Browser.Navigation.pushUrl mod.key <| Url.toString url
                    )

                Nothing ->
                    ( mod, Cmd.none )
