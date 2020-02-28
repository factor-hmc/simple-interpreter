module Foogle exposing (..)

import Browser
import Html.Styled exposing (Attribute, Html, div, h2, input, text, toUnstyled)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (on, onInput, preventDefaultOn)
import Http
import Json.Decode as JD
import Url.Builder as Url


endpoint : String
endpoint =
    "http://foogle-server.herokuapp.com"


type alias SearchResult =
    { name : String
    , effect : String
    }


type alias Model =
    { query : String
    , numResults : Int
    , searchResults : Result Http.Error (List SearchResult)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { query = ""
      , numResults = 5
      , searchResults = Ok []
      }
    , Cmd.none
    )


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
                { url = Url.crossOrigin endpoint [] [ Url.string "search" model.query, Url.int "numResults" model.numResults ]
                , expect = Http.expectJson GotSearchResults searchResultsDecoder
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
            JD.map2 (\name effect -> { name = name, effect = effect })
                (JD.field "name" JD.string)
                (JD.field "effect" JD.string)
    in
    JD.list searchResultDecoder


view : Model -> Html Msg
view model =
    div
        []
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
            div [] [ text (": " ++ res.name ++ " " ++ res.effect) ]
    in
    div
        []
        (case searchResults of
            Ok reses ->
                List.map viewSearchResult reses

            Err err ->
                case err of
                    Http.BadBody e ->
                        [ text <| "Error loading search results: " ++ e ]

                    Http.BadUrl url ->
                        [ text <| "Error loading search results: bad url " ++ url ]

                    Http.Timeout ->
                        [ text "Foogle server timed out." ]

                    Http.NetworkError ->
                        [ text "Foogle server network error." ]

                    _ ->
                        [ text "Error loading search results." ]
        )


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


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view >> toUnstyled
        }
