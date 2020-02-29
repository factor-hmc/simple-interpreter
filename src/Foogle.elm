module Foogle exposing (..)

import Browser
import Html.Styled exposing (Attribute, Html, a, div, h2, input, pre, text, toUnstyled)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (on, onInput, preventDefaultOn)
import Http
import Json.Decode as JD
import Url.Builder as Url


endpoint : String
endpoint =
    "https://foogle-server.herokuapp.com"


type alias SearchResult =
    { name : String
    , effect : String
    , url : String
    }


type alias Model =
    { query : String
    , numResults : Int
    , searchResults : Result Http.Error (List SearchResult)
    }


init : Model
init =
    { query = ""
    , numResults = 5
    , searchResults = Ok []
    }


type Msg
    = Search
    | UpdateQuery String
    | GotSearchResults (Result Http.Error (List SearchResult))
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( model
            , Http.get
                { url =
                    Url.crossOrigin endpoint
                        []
                        [ Url.string "search" model.query
                        , Url.int "numResults" model.numResults
                        ]
                , expect = expectSearchResults GotSearchResults
                }
            )

        GotSearchResults results ->
            ( { model | searchResults = results }, Cmd.none )

        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        Nop ->
            ( model, Cmd.none )


searchResultsDecoder : JD.Decoder (List SearchResult)
searchResultsDecoder =
    let
        searchResultDecoder =
            JD.map3 (\name effect url -> { name = name, effect = effect, url = url })
                (JD.field "name" JD.string)
                (JD.field "effect" JD.string)
                (JD.field "url" JD.string)
    in
    JD.list searchResultDecoder


view : Model -> List (Html Msg)
view model =
    [ h2 [] [ text "Foogle" ]
    , input
        [ placeholder "Search Foogle"
        , value model.query
        , onInput UpdateQuery
        , preventDefaultOn "keydown" eventKey
        ]
        []
    , viewSearchResults model.searchResults
    ]


viewSearchResults : Result Http.Error (List SearchResult) -> Html Msg
viewSearchResults searchResults =
    let
        viewSearchResult res =
            div [ class "result" ] [ a 
                                       [ class "result-link", href res.url ]
                                       [ text (": " ++ res.name ++ " " ++ res.effect) ]
                                   ]
    in
    div
        [ class "results" ]
        (case searchResults of
            Ok reses ->
                List.map viewSearchResult reses

            Err err ->
                case err of
                    Http.BadBody e ->
                        [text <| "Error loading search results:", pre [] [text e]]

                    Http.BadUrl url ->
                        [ text <| "Error loading search results: bad url " ++ url ]

                    Http.Timeout ->
                        [ text "Foogle server timed out." ]

                    Http.NetworkError ->
                        [ text "Foogle server network error." ]

                    _ ->
                        [ text "Error loading search results." ]
        )

expectSearchResults : (Result Http.Error (List SearchResult) -> msg) -> Http.Expect msg
expectSearchResults toMsg =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (Http.BadUrl url)

        Http.Timeout_ ->
          Err Http.Timeout

        Http.NetworkError_ ->
          Err Http.NetworkError

        Http.BadStatus_ metadata body ->
          Err (Http.BadBody <| 
                "Error " 
                ++ String.fromInt metadata.statusCode
                ++ "\n"
                ++ body
              )

        Http.GoodStatus_ metadata body ->
          case JD.decodeString searchResultsDecoder body of
            Ok value ->
              Ok value

            Err err ->
              Err (Http.BadBody (JD.errorToString err))

eventKey : JD.Decoder ( Msg, Bool )
eventKey =
    JD.field "key" JD.string
        |> JD.map
            (\key ->
                case key of
                    "Enter" ->
                        ( Search, True )

                    _ ->
                        ( Nop, False )
            )



--
--main =
--    Browser.element
--        { init = init
--        , update = update
--        , subscriptions = \_ -> Sub.none
--        , view = view >> toUnstyled
--        }
