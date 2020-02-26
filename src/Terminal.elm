module Terminal exposing (..)

import Browser.Dom as Dom
import Css
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
    , width : Maybe Float
    }


type alias Model =
    { history : List Snapshot
    , current : Snapshot
    }


type Msg
    = Input String
    | Resize Float
    | Enter
    | Focus
    | Nop


init : ( Model, Cmd Msg )
init =
    ( { history = []
      , current =
            { input = ""
            , state = Eval.init
            , width = Nothing
            }
      }
    , focusPrompt
    )


setInput : String -> Model -> Model
setInput s mod =
    { mod | current = (\snap -> { snap | input = s }) mod.current }


resizeInput : Cmd Msg
resizeInput =
    Dom.getViewportOf "prompt-width"
        |> Task.attempt (Result.map (.scene >> .width >> Resize) >> Result.withDefault Nop)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mod =
    case msg of
        Resize w ->
            ( { mod | current = (\snap -> { snap | width = Just w }) mod.current }, Cmd.none )

        Input s ->
            ( setInput s mod, Cmd.batch [ resizeInput, focusPrompt ] )

        Enter ->
            ( mod.current.input
                |> Parser.run (FactorParser.words |. Parser.end)
                |> Result.mapError
                    (\e ->
                        let
                            _ =
                                Debug.log "parserr" e
                        in
                        "parseror"
                    )
                --(always "parser error")
                |> Result.andThen (Eval.evalWords mod.current.state)
                |> Result.map
                    (\st ->
                        { mod
                            | history = mod.current :: mod.history
                            , current =
                                { input = ""
                                , state = st
                                , width = Nothing
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



--(always Nop)


eventKey : JD.Decoder ( Msg, Bool )
eventKey =
    JD.field "key" JD.string
        |> JD.map
            (\key ->
                case key of
                    "Enter" ->
                        ( Enter, True )

                    _ ->
                        ( Nop, False )
            )


viewSnapshot : Bool -> Snapshot -> Html Msg
viewSnapshot active snap =
    div
        [ class "snapshot" ]
        [ snap.state.output
            |> Maybe.map
                (\s ->
                    div
                        [ class "output" ]
                        [ text s ]
                )
            |> Maybe.withDefault (text "")
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
                ((case snap.width of
                    Just h ->
                        [ Attr.css [ Css.important <| Css.width <| Css.px h ] ]

                    Nothing ->
                        []
                 )
                    ++ (case active of
                            True ->
                                [ id "prompt"
                                , Events.onInput Input
                                , Events.preventDefaultOn "keydown" eventKey
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
        , span [ id "prompt-width" ] [ text mod.current.input ]
        ]
