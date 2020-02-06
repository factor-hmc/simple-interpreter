module App exposing (..)

--import Styles

import Book
import Browser
import Browser.Dom
import Browser.Events exposing (onAnimationFrame)
import Browser.Navigation
import Dict
import Eval
import FactorParser
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A
    exposing
        ( class
        , css
        , disabled
        , href
        , id
        , placeholder
        , property
        , src
        , value
        )
import Html.Styled.Events
    exposing
        ( on
        , onClick
        , onInput
        , onMouseDown
        , onMouseOut
        , onMouseOver
        , onMouseUp
        )
import Http
import Json.Decode as J
import Json.Encode
import Lang exposing (..)
import Logo
import Parser exposing ((|.))
import Pretty
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes exposing (d, fill)
import Task
import Time
import Url


type alias Snapshot =
    { input : String
    , state : Eval.State
    , output : String
    }


type alias Anim =
    { init : Float
    , current : Float
    , target : Float
    , start : Float
    , duration : Float
    }


resetAnim : Float -> Float -> Float -> Anim -> Anim
resetAnim time duration target anim =
    { anim
        | init = anim.current
        , target = target
        , start = time
        , duration = duration
    }


updateAnim : Float -> Anim -> Anim
updateAnim time anim =
    { anim
        | current =
            anim.init
                + (if time < anim.start then
                    0

                   else if time > anim.start + anim.duration then
                    1

                   else
                    (time - anim.start)
                        / anim.duration
                  )
                * (anim.target - anim.init)
    }


type alias Model =
    { history : List Snapshot
    , current : Snapshot
    , book : Book.Book
    , time : Float
    , logo : Anim
    }


type Logo
    = Hover
    | Press
    | Release
    | Out


type Msg
    = Input String
    | Enter
    | Frame Float
    | Logo Logo
    | Focus
    | Load (Result Http.Error String)
    | Nop


main =
    Browser.application
        { init = init
        , onUrlChange = \_ -> Nop
        , onUrlRequest = \_ -> Nop
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled >> List.singleton >> Browser.Document "Factor"
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--onAnimationFrame <|
--    \t -> Frame <| toFloat (Time.posixToMillis t) / 1000


focusPrompt : Cmd Msg
focusPrompt =
    Browser.Dom.focus "prompt"
        |> Task.attempt (always Nop)


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () _ _ =
    ( { history = []
      , current =
            { input = ""
            , state = Eval.init
            , output = ""
            }
      , time = 0
      , logo =
            { init = 0
            , current = 0
            , target = 0
            , start = 0
            , duration = 1
            }
      , book = Book.init
      }
    , Cmd.batch
        [ focusPrompt
        , Http.get
            { url = "https://factor-book.netlify.com/json/README.json"
            , expect = Http.expectString Load
            }
        ]
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

        Frame t ->
            ( { model
                | time = t
                , logo = updateAnim model.time model.logo
              }
            , Cmd.none
            )

        Focus ->
            ( model, focusPrompt )

        Logo e ->
            ( let
                ( offset, dur, target ) =
                    case e of
                        Hover ->
                            ( 5, 0.5, -0.4 )

                        Press ->
                            ( 0, 0.1, 0.4 )

                        Release ->
                            ( 0, 0.1, 0 )

                        Out ->
                            ( 0, 0.1, 0 )
              in
              { model
                | logo = resetAnim (model.time + offset) dur target model.logo
              }
            , Cmd.none
            )

        Load (Err _) ->
            ( model, Cmd.none )

        Load (Ok s) ->
            ( { model | book = Book.update s model.book }, Cmd.none )

        Enter ->
            case Parser.run (FactorParser.words |. Parser.end) model.current.input of
                Err e ->
                    ( model, Cmd.none )

                Ok words ->
                    case Eval.evalWords model.current.state words of
                        Err r ->
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
                    ++ [ value snap.input ]
                )
                []
            ]
        ]


view : Model -> Html Msg
view model =
    main_
        [ id "main" ]
        [ div
            [ id "sidebar" ]
            [ div
                [ onMouseOver <| Logo Hover
                , onMouseDown <| Logo Press
                , onMouseUp <| Logo Release
                , onMouseOut <| Logo Out
                , id "logo"
                ]
                [ Logo.view model.logo.current ]
            , nav
                [ id "menu" ]
                [ div
                    [ id "summary" ]
                    [ Book.viewSummary model.book.summary ]
                ]
            ]
        , div
            [ id "book" ]
            [ Book.viewPage model.book.page
            ]
        , div
            [ id "terminal"
            , onClick Focus
            ]
            [ div [ id "terminal-scroll" ]
                [ div [ id "terminal-content" ]
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
