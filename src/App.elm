module App exposing (..)

import Browser
import Css exposing (..)
import Dict
import Eval
import FactorParser
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (css, href, placeholder, src, value)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as J
import Lang exposing (..)
import Parser exposing ((|.))
import Pretty



--MAIN


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }



--MODEL


type alias Snapshot =
    { input : String
    , state : Eval.State
    , --not sure
      output : String
    }


type alias Model =
    { history : List Snapshot
    , current : Snapshot
    }


type Msg
    = Input String
    | Enter
    | Nop


init : Model
init =
    { history = []
    , current =
        { input = ""
        , state = Eval.init
        , output = ""
        }
    }



--UPDATE
--simply updating the type


update : Msg -> Model -> Model
update msg model =
    let
        setInput str snap =
            { snap | input = str }
    in
    case msg of
        Input str ->
            { model
                | current = setInput str model.current
            }

        Nop ->
            model

        Enter ->
            case Parser.run (FactorParser.words |. Parser.end) model.current.input of
                Err e ->
                    let
                        _ =
                            Debug.log "parse error" e
                    in
                    model

                Ok words ->
                    case Eval.evalWords model.current.state words of
                        Err r ->
                            let
                                _ =
                                    Debug.log "eval error" r
                            in
                            model

                        Ok sta ->
                            { model
                                | history = model.current :: model.history
                                , current =
                                    { input = ""
                                    , state = sta
                                    , output = ""
                                    }
                            }



--VIEW


view : Model -> Html Msg
view model =
    div
        [ css [ backgroundColor (rgb 244 239 217), height (px 1024) ] ]
        [ div []
            (model.current.state.stack
                |> List.map
                    (\lit ->
                        div
                            [ css [ marginLeft (px 512) ] ]
                            [ text (Pretty.showLiteral lit)
                            ]
                    )
            )
        , input
            [ css [ marginLeft (px 512) ]
            , placeholder "Input Factor Code"
            , value model.current.input
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
            []
        ]
