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
import Html.Styled as Html exposing (..)
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
import Terminal
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
    { terminal : Terminal.Model
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
    = Frame Float
    | Terminal Terminal.Msg
    | Logo Logo
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


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () _ _ =
    let
        ( term, termCmd ) =
            Terminal.init
    in
    ( { terminal = term
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
        [ termCmd |> Cmd.map Terminal
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
        Frame t ->
            ( { model
                | time = t
                , logo = updateAnim model.time model.logo
              }
            , Cmd.none
            )

        Terminal m ->
            let
                ( tm, c ) =
                    Terminal.update m model.terminal
            in
            ( { model | terminal = tm }, c |> Cmd.map Terminal )

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

        Nop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    main_
        [ id "main" ]
        [ div
            [ id "sidebar" ]
            [ div
                [ id "title" ]
                [ div
                    [ onMouseOver <| Logo Hover
                    , onMouseDown <| Logo Press
                    , onMouseUp <| Logo Release
                    , onMouseOut <| Logo Out
                    , id "logo"
                    ]
                    [ Logo.view model.logo.current
                    ]
                , img [ src "/static/logo-text.svg" ] []
                ]
            , nav
                [ id "menu" ]
                [ section
                    [ id "summary" ]
                    [ h1 [] [ text "Tutorials" ]
                    , Book.viewSummary model.book.summary
                    ]
                , section
                    [ id "hoogle" ]
                    [ h1 [] [ text "Foogle" ] ]
                ]
            ]
        , div
            [ id "book" ]
            [ Book.viewPage model.book.page |> Html.map Terminal
            ]
        , Terminal.view model.terminal |> Html.map Terminal
        ]
