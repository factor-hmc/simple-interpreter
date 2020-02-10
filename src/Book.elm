module Book exposing (..)

import Html.Parser
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (id, property)
import Html.Styled.Events as Ev
import Json.Decode as JD exposing (..)
import Json.Encode
import Terminal


type alias File =
    { path : String
    , mtime : String
    }


type alias Article =
    { title : String
    , level : String
    , depth : Int
    }


type alias Part =
    { title : String
    , articles : List Article
    }


type alias Summary =
    { file : File
    , parts : List Part
    }


type alias Page =
    { title : String
    , level : String
    , depth : Int
    , content : String
    }


type alias Book =
    { summary : Summary
    , page : Page
    }


init : Book
init =
    { summary =
        { file =
            { path = ""
            , mtime = ""
            }
        , parts = []
        }
    , page =
        { title = ""
        , level = ""
        , depth = 0
        , content = ""
        }
    }


file : Decoder File
file =
    map2 File
        (field "path" string)
        (field "mtime" string)


summary : Decoder Summary
summary =
    map2 Summary
        (field "file" file)
        (field "parts" <|
            list <|
                map2 Part
                    (field "title" string)
                    (field "articles" <|
                        list <|
                            map3 Article
                                (field "title" string)
                                (field "level" string)
                                (field "depth" int)
                    )
        )


page : Decoder Page
page =
    map4 Page
        (field "title" string)
        (field "level" string)
        (field "depth" int)
        (field "content" string)


book : Decoder Book
book =
    map2 Book
        (field "summary" summary)
        (field "page" page)


viewSummary : Summary -> Html msg
viewSummary =
    div []
        << List.map
            (\part ->
                div []
                    [ div
                        []
                        (part.articles
                            |> List.map (\art -> div [] [ text art.title ])
                        )
                    ]
            )
        << .parts


textContent : Html.Parser.Node -> String
textContent n =
    case n of
        Html.Parser.Text s ->
            s

        Html.Parser.Element _ _ c ->
            List.map textContent c |> String.concat

        Html.Parser.Comment _ ->
            ""


viewNode : (String -> msg) -> Html.Parser.Node -> Html msg
viewNode copy n =
    let
        attr ( k, v ) =
            case k of
                "class" ->
                    Attr.class v

                _ ->
                    property k <| Json.Encode.string v

        elem t a c =
            Html.node t (List.map attr a) <| List.map (viewNode copy) c
    in
    case n of
        Html.Parser.Text s ->
            Html.text s

        Html.Parser.Element t a c ->
            case t of
                "pre" ->
                    Html.div
                        [ Attr.class "code-block" ]
                        [ Html.pre [] <| List.map (viewNode copy) c
                        , button
                            [ Ev.onClick <|
                                copy (List.map textContent c |> String.concat)
                            ]
                            [ text "➦" ]
                        ]

                _ ->
                    elem t a c

        Html.Parser.Comment _ ->
            Html.text ""



--viewContent : List Html.Parser.Node -> List (Html Terminal.Msg)
--viewContent =
--    List.map viewNode


viewPage : (String -> msg) -> Page -> Html msg
viewPage copy p =
    div
        [ id "book-content" ]
        (Html.Parser.run p.content
            |> Result.map (List.map <| viewNode copy)
            |> Result.withDefault []
        )


update : String -> Book -> Book
update str b =
    decodeString book str
        |> Result.withDefault b
