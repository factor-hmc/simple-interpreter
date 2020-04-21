module App exposing (..)

import Book
import Browser
import Browser.Events exposing (onAnimationFrame)
import Browser.Navigation
import Foogle
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Html.Styled.Events as Events exposing (..)
import Http
import Logo
import Nav
import Task
import Terminal
import Time
import Url


type alias Anim =
    { init : Float
    , current : Float
    , target : Float
    , start : Float
    , duration : Float
    , active : Bool
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
        , active = time < anim.start + anim.duration
    }


type alias Model =
    { terminal : Terminal.Model
    , book : Book.Book
    , time : Float
    , logo : Anim
    , key : Browser.Navigation.Key
    , foogle : Foogle.Model
    , nav : Nav.Model
    }


type Logo
    = Hover
    | Press
    | Release
    | Out


type Msg
    = Frame Float
    | Copy String
    | Terminal Terminal.Msg
    | Logo Logo
    | Trigger Logo Float
    | Load (Result Http.Error String)
    | Nav Browser.UrlRequest
    | Foogle Foogle.Msg
    | Nop


main =
    Browser.application
        { init = init
        , onUrlChange = \_ -> Nop
        , onUrlRequest = Nav
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled >> List.singleton >> Browser.Document "Factor"
        }


subscriptions : Model -> Sub Msg
subscriptions mod =
    if mod.logo.active then
        onAnimationFrame <|
            \t -> Frame <| toFloat (Time.posixToMillis t) / 1000

    else
        Sub.none


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () _ key =
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
            , active = False
            }
      , foogle = Foogle.init
      , book = Book.init
      , key = key
      , nav = Nav.init
      }
    , Cmd.batch
        [ termCmd |> Cmd.map Terminal
        , Http.get
            { url = "https://factor-book.netlify.app/json/README.json"
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
        Trigger e t ->
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
                | logo = resetAnim (t + offset) dur target model.logo
                , time = t
              }
            , Cmd.none
            )

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
            let
                activate anim =
                    { anim | active = True }
            in
            ( { model | logo = activate model.logo }
            , Task.perform (\t -> Trigger e (toFloat (Time.posixToMillis t) / 1000)) Time.now
            )

        Load (Err _) ->
            ( model, Cmd.none )

        Load (Ok s) ->
            ( { model | book = Book.update s model.book }, Cmd.none )

        Copy s ->
            let
                ( t, c ) =
                    Terminal.update (Terminal.Input s) model.terminal
            in
            ( { model | terminal = t }, c |> Cmd.map Terminal )

        Nav (Browser.Internal u) ->
            let
                ( n, c ) =
                    Nav.update Load model.key u model.nav
            in
            ( { model | nav = n }, c )

        Nav (Browser.External u) ->
            ( model, Cmd.none )

        Foogle m ->
            let
                ( f, c ) =
                    Foogle.update m model.foogle
            in
            ( { model | foogle = f }, Cmd.map Foogle c )

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
                , img [ src "/static/logo-text-online.svg" ] []
                ]
            , nav
                [ id "menu" ]
                [ section []
                    [ h1 [] [ a [ href "/book/README.md" ] [ text "Tutorials" ] ]
                    , Book.viewSummary model.book.summary
                    ]
                , section []
                    [ h1 [] [ a [ href "/foogle" ] [ text "Foogle" ] ] ]
                ]
            ]
        , div [ id "app" ]
            [ div
                [ id "book"
                , Attr.classList
                    [ ( "app", True )
                    , ( "active", model.nav.app /= Nav.Foogle )
                    ]
                ]
                [ Book.viewPage Copy model.book.page ]
            , div
                [ id "foogle"
                , Attr.classList
                    [ ( "app", True )
                    , ( "active", model.nav.app == Nav.Foogle )
                    ]
                ]
                (Foogle.view model.foogle |> List.map (Html.map Foogle))
            ]
        , Terminal.view model.terminal |> Html.map Terminal
        ]
