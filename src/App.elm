module App exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Dict
import Eval
import FactorParser
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (css, disabled, href, id, placeholder, src, value)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as J
import Lang exposing (..)
import Parser exposing ((|.))
import Pretty
import Styles
import Task
import Url


type alias Snapshot =
    { input : String
    , state : Eval.State
    , output : String
    }


type alias Model =
    { history : List Snapshot
    , current : Snapshot
    }


type Msg
    = Input String
    | Enter
    | Nop


main =
    Browser.application
        { init = init
        , onUrlChange = \_ -> Nop
        , onUrlRequest = \_ -> Nop
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view >> toUnstyled >> List.singleton >> Browser.Document "Factor"
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () _ _ =
    ( { history = []
      , current =
            { input = ""
            , state = Eval.init
            , output = ""
            }
      }
    , Browser.Dom.focus "prompt"
        |> Task.attempt (always Nop)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setInput str snap =
            { snap | input = str }
    in
    case msg of
        Input str ->
            ( { model | current = setInput str model.current }, Cmd.none )

        Nop ->
            ( model, Cmd.none )

        Enter ->
            case Parser.run (FactorParser.words |. Parser.end) model.current.input of
                Err e ->
                    let
                        _ =
                            Debug.log "parse error" e
                    in
                    ( model, Cmd.none )

                Ok words ->
                    case Eval.evalWords model.current.state words of
                        Err r ->
                            let
                                _ =
                                    Debug.log "eval error" r
                            in
                            ( model, Cmd.none )

                        Ok st ->
                            ( { model
                                | history = model.current :: model.history
                                , current =
                                    { input = ""
                                    , state = st
                                    , output = ""
                                    }
                              }
                            , Cmd.none
                            )


viewSnapshot : Bool -> Snapshot -> Html Msg
viewSnapshot active snap =
    div
        [ css Styles.snapshot ]
        [ div
            [ css Styles.output ]
            [ text snap.output ]
        , case snap.state.stack of
            [] ->
                text ""

            st ->
                div
                    [ css Styles.stack ]
                    (st
                        |> List.reverse
                        |> List.map
                            (div
                                [ css Styles.lit ]
                                << (Pretty.showLiteral >> text >> List.singleton)
                            )
                    )
        , div
            [ css Styles.inputLine ]
            [ input
                ((case active of
                    True ->
                        [ id "prompt"
                        , onInput Input
                        , on "keydown"
                            (J.field "key" J.string
                                |> J.map
                                    (\key ->
                                        case key of
                                            "Enter" ->
                                                Enter

                                            _ ->
                                                Nop
                                    )
                            )
                        ]

                    False ->
                        [ disabled True ]
                 )
                    ++ [ value snap.input
                       , css Styles.input
                       ]
                )
                []
            ]
        ]


view : Model -> Html Msg
view model =
    main_
        [ css Styles.main_ ]
        [ img
            [ src "logo.svg", css Styles.logo ]
            []
        , div
            [ id "terminal"
            , css Styles.terminal
            ]
            [ div [ css Styles.terminalScroll ]
                [ div [ css Styles.terminalContent ]
                    [ div []
                        (model.history
                            |> List.reverse
                            |> List.map (viewSnapshot False)
                        )
                    , viewSnapshot True model.current
                    ]
                ]
            ]
        ]
