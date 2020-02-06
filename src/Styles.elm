module Styles exposing (..)

import Css exposing (..)


main_ : List Style
main_ =
    [ position absolute
    , left <| rem 0
    , top <| rem 0
    , right <| rem 0
    , bottom <| rem 0
    , displayFlex
    , justifyContent center
    ]

sidebar : List Style
sidebar =
    [ padding <| rem 1
    , backgroundColor <| rgb 53 53 53
    ]

logoCircle : List Style
logoCircle =
    [ borderRadius <| pct 50
    , overflow hidden
    ]

logo : List Style
logo =
    [ width <| rem 6
    , alignSelf flexStart
    ]

bookContent : List Style
bookContent =
    [ position absolute
    , left <| rem 0
    , top <| rem 0
    , height <| pct 100
    , width <| pct 100
    , padding <| rem 0
    , margin <| rem 0
    , borderStyle none
    ]

book : List Style
book =
    [ flexGrow <| num 1
    , position relative
    ]

terminal : List Style
terminal =
    [ fontFamilies [ "Roboto Mono", "monospace" ]
    , fontSize <| rem 0.875
    , minWidth <| rem 40
    --, backgroundColor <| rgb 240 240 240
    , lineHeight <| em 1.25
    , borderRadius <| rem 0.25
    , overflow hidden
    , displayFlex
    , border3 (px 1) solid (rgb 180 180 180)
    --, boxShadow4 (rem 0) (rem 0) (rem 0.25) (rgb 25 25 25)
    , margin <| rem 2
    ]


terminalScroll : List Style
terminalScroll =
    [ flexGrow <| num 1
    , overflowY scroll
    ]


terminalContent : List Style
terminalContent =
    [ displayFlex
    , flexDirection column
    , padding <| rem 0.5
    ]


inputLine : List Style
inputLine =
    [ before
        [ property "content" "'IN: scratchpad'"
        , backgroundColor <| rgb 255 179 179
        , marginRight <| rem 0.5
        ]
    , displayFlex
    , alignItems baseline
    ]


input : List Style
input =
    [ borderStyle none
    , padding <| rem 0
    , backgroundColor transparent
    , fontFamily inherit
    , fontSize inherit
    , lineHeight inherit
    , color inherit
    , flexGrow <| num 1
    , outline none
    , boxShadow none
    , property "caret-color" "red"
    , selection
        [ backgroundColor <| rgb 204 204 255
        , color inherit
        ]
    ]


output : List Style
output =
    []


stack : List Style
stack =
    [ before
        [ property "content" "'--- Data stack:'"
        , color <| rgb 128 128 128
        ]
    , marginTop <| em 1.25
    ]


lit : List Style
lit =
    []


snapshot : List Style
snapshot =
    []
