module App exposing (..)

import Browser
import Eval
import FactorParser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on)
import Json.Decode as J
import Lang
import Parser
import Pretty



--MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



--MODEL


type alias Snapshot =
    { input : String
    , stack : Lang.Stack
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
        , stack = []
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
            case Parser.run FactorParser.words model.current.input of
                Err e ->

                    model
            

                Ok words ->
                    case Eval.evalWords model.current.stack words of
                        Err r ->


                            model

                        Ok sta ->
                            { model
                                | history = model.current :: model.history
                                , current =
                                    { input = ""
                                    , stack = sta
                                    , output = ""
                                    }
                            }



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            (model.current.stack
                |> List.map (\lit -> div [] [ text (Pretty.showLiteral lit) ])
            )
        , input
            [ placeholder "Input Factor Code"
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
