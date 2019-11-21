
module App exposing (..)
import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

--MAIN

main = Browser.sandbox{init = init, update = update, view = view}


--MODEL

type alias Model = {
       content : String
    }

init : Model
init = 
    {content = ""}


--UPDATE

type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent ++ " will be compilied to factor at some point!" }

            





--VIEW

view : Model -> Html Msg
view model = 
    div[]
        [input[placeholder "Input Factor Code", value model.content, onInput Change][]
        , div [] [text (model.content)]
        ]
