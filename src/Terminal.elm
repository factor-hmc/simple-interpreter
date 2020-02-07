module Terminal exposing (..)

import Browser.Dom as Dom
import Eval
import FactorParser
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Html.Styled.Events as Events exposing (on)
import Json.Decode as JD
import Parser exposing ((|.))
import Pretty
import Task


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
    | Focus
    | Nop


init : ( Model, Cmd Msg )
init =
    ( { history = []
      , current =
            { input = ""
            , state = Eval.init
            , output = ""
            }
      }
    , focusPrompt
    )


setInput : String -> Snapshot -> Snapshot
setInput s snap =
    { snap | input = s }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mod =
    case msg of
        Input s ->
            ( { mod | current = setInput s mod.current }, Cmd.none )

        Enter ->
            ( mod.current.input
                |> Parser.run (FactorParser.words |. Parser.end)
                |> Result.mapError (always "parser error")
                |> Result.andThen (Eval.evalWords mod.current.state)
                |> Result.map
                    (\st ->
                        { mod
                            | history = mod.current :: mod.history
                            , current =
                                { input = ""
                                , state = st
                                , output = ""
                                }
                        }
                    )
                |> Result.withDefault mod
            , Dom.getViewportOf "terminal-scroll"
                |> Task.andThen
                    (.scene
                        >> .height
                        >> Dom.setViewportOf "terminal-scroll" 0
                    )
                |> Task.attempt (always Nop)
            )

        Focus ->
            ( mod, focusPrompt )

        Nop ->
            ( mod, Cmd.none )


focusPrompt : Cmd Msg
focusPrompt =
    Dom.focus "prompt"
        |> Task.attempt (always Nop)


eventKey : JD.Decoder Msg
eventKey =
    JD.field "key" JD.string
        |> JD.map
            (\key ->
                case key of
                    "Enter" ->
                        Enter

                    _ ->
                        Nop
            )


viewSnapshot : Bool -> Snapshot -> Html Msg
viewSnapshot active snap =
    div
        [ class "snapshot" ]
        [ div
            [ class "output" ]
            [ text snap.output ]
        , case snap.state.stack of
            [] ->
                text ""

            st ->
                div
                    [ class "stack" ]
                    (st
                        |> List.reverse
                        |> List.map
                            (div
                                [ class "stack-element" ]
                                << (Pretty.showLiteral >> text >> List.singleton)
                            )
                    )
        , div
            [ class "input" ]
            [ input
                ((case active of
                    True ->
                        [ id "prompt"
                        , Events.onInput Input
                        , on "keydown" eventKey
                        ]

                    False ->
                        [ disabled True ]
                 )
                    ++ [ value snap.input ]
                )
                []
            ]
        ]


view : Model -> Html Msg
view mod =
    div
        [ id "terminal"
        , Events.onClick Focus
        ]
        [ div [ id "terminal-scroll" ]
            [ div [ id "terminal-content" ]
                [ div []
                    (mod.history
                        |> List.reverse
                        |> List.map (viewSnapshot False)
                    )
                , viewSnapshot True mod.current
                ]
            ]
        ]
