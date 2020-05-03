module Book exposing (..)

import Book.Url
import Html.Parser
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (id, property)
import Html.Styled.Events as Ev
import Json.Decode as JD exposing (..)
import Json.Encode
import Terminal
import Url
import Url.Builder
import Url.Parser exposing ((</>))


type alias File =
    { path : String
    , mtime : String
    , type_ : String
    }


type alias Article =
    { title : String
    , level : String
    , depth : Int
    , path : String
    , ref : String
    , articles : Articles
    }


type Articles
    = Articles (List Article)


type alias Part =
    { title : String
    , articles : List Article
    }


type alias Summary =
    { file : File
    , parts : List Part
    }


type alias Readme =
    { file : File
    }


type alias Page =
    { title : String
    , level : String
    , depth : Int
    , previous : Maybe Article
    , next : Maybe Article
    , content : String
    }


type alias Book =
    { summary : Summary
    , readme : Readme
    , page : Page
    , file : File
    , version : String
    }


initFile : File
initFile =
    { path = ""
    , mtime = ""
    , type_ = ""
    }


init : Book
init =
    { summary =
        { file = initFile
        , parts = []
        }
    , readme = { file = initFile }
    , page =
        { title = ""
        , level = ""
        , depth = 0
        , previous = Nothing
        , next = Nothing
        , content = ""
        }
    , file = initFile
    , version = ""
    }


file : Decoder File
file =
    map3 File
        (field "path" string)
        (field "mtime" string)
        (field "type" string)


article : Decoder Article
article =
    map6 Article
        (field "title" string)
        (field "level" string)
        (field "depth" int)
        (field "path" string)
        (field "ref" string)
        (field "articles" <|
            JD.map Articles <|
                list <|
                    lazy <|
                        \() -> article
        )


summary : Decoder Summary
summary =
    map2 Summary
        (field "file" file)
        (field "parts" <|
            list <|
                map2 Part
                    (field "title" string)
                    (field "articles" <| list article)
        )


page : Decoder Page
page =
    map6 Page
        (field "title" string)
        (field "level" string)
        (field "depth" int)
        (maybe <| field "previous" article)
        (maybe <| field "next" article)
        (field "content" string)


book : Decoder Book
book =
    map5 Book
        (field "summary" summary)
        (field "readme" <| JD.map Readme <| field "file" file)
        (field "page" page)
        (field "file" file)
        (field "version" string)


viewArticle : Article -> Html msg
viewArticle art =
    let
        (Articles children) =
            art.articles
    in
    li
        []
        [ a [ Attr.href <| "/book/" ++ art.path ]
            [ text art.title ]
        , ul [] <| List.map viewArticle children
        ]


viewSummary : Summary -> Html msg
viewSummary =
    div []
        << List.map
            (\part ->
                div []
                    [ ul [] <| List.map viewArticle part.articles ]
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


viewNode : (String -> msg) -> List String -> Html.Parser.Node -> Html msg
viewNode copy path n =
    let
        attr ( k, v ) =
            case k of
                "class" ->
                    Attr.class v

                _ ->
                    property k <| Json.Encode.string v

        elem t a c =
            Html.node t (List.map attr a) <| List.map (viewNode copy path) c

        modifyAttr km f ( k, v ) =
            ( k
            , if k == km then
                f v

              else
                v
            )

        isJust a =
            case a of
                Just _ ->
                    True

                Nothing ->
                    False

        rewriteUrl p =
            -- if p parses as a URL, then it is an absolute/external
            -- path, and we leave it alone; otherwise we set it to a
            -- relative path
            if isJust <| Url.fromString p then
                p

            else
                Url.Builder.crossOrigin "https://factor-book.netlify.app"
                    (if String.startsWith "/" p then
                        -- abs path
                        [ p ]

                     else
                        -- rel path
                        List.reverse path
                            |> List.tail
                            |> Maybe.withDefault []
                            |> (::) p
                            |> List.reverse
                    )
                    []

        isFactor c =
            case c of
                [ Html.Parser.Element "code" [ ( "class", "lang-factor" ) ] _ ] ->
                    True

                _ ->
                    False
    in
    case n of
        Html.Parser.Text s ->
            Html.text s

        Html.Parser.Element t a c ->
            case t of
                "pre" ->
                    Html.div
                        [ Attr.class "code-block" ]
                        [ Html.pre [] <| List.map (viewNode copy path) c
                        , if isFactor c then
                            button
                                [ Ev.onClick <|
                                    copy (List.map textContent c |> String.concat)
                                ]
                                [ text "➦" ]

                          else
                            text ""
                        ]

                "img" ->
                    Html.img
                        (List.map (attr << modifyAttr "src" rewriteUrl) a)
                    <|
                        List.map (viewNode copy path) c

                _ ->
                    elem t a c

        Html.Parser.Comment _ ->
            Html.text ""


viewPage : (String -> msg) -> List String -> Page -> Html msg
viewPage copy path p =
    div
        [ id "book-content" ]
        (Html.Parser.run p.content
            |> Result.map (List.map <| viewNode copy path)
            |> Result.withDefault []
        )


update : String -> Book -> Book
update str b =
    decodeString book str
        |> Result.withDefault b
