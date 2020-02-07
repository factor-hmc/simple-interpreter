module Book exposing (..)

import Html.Parser
import Html.Parser.Util
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id, property)
import Json.Decode as JD exposing (..)
import Json.Encode


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


viewPage : Page -> Html msg
viewPage p =
    div
        [ id "book-content" ]
        (Html.Parser.run p.content
            |> Result.map (Html.Parser.Util.toVirtualDom >> List.map fromUnstyled)
            |> Result.withDefault []
        )


update : String -> Book -> Book
update str b =
    decodeString book str
        |> Result.withDefault b
