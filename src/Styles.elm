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
    , padding2 (rem 4) (rem 0)
    , justifyContent center
    , backgroundColor <| rgb 53 53 53
    ]

logoCircle : List Style
logoCircle =
    [ borderRadius <| pct 50
    , overflow hidden
    ]

logo : List Style
logo =
    [ width <| rem 8
    , alignSelf flexStart
    ]


terminal : List Style
terminal =
    [ fontFamilies [ "Roboto Mono", "monospace" ]
    , fontSize <| rem 0.875
    , minWidth <| rem 60
    , backgroundColor <| rgb 240 240 240
    , marginLeft <| rem 1
    , lineHeight <| em 1.25
    , borderRadius <| rem 0.25
    , overflow hidden
    , displayFlex
    , boxShadow4 (px 0) (rem 0.25) (rem 0.5) (rgb 25 25 25)
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
