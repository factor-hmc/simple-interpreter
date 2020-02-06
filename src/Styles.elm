module Styles exposing (..)

import Css exposing (..)



bookContent : List Style
bookContent =
    [ --position absolute
    --, left <| rem 0
    --, top <| rem 0
    --, height <| pct 100
    --, width <| pct 100
    --, padding <| rem 0
    --, margin <| rem 0
     overflowY scroll
    , padding <| rem 2
    , flexGrow <| num 1
    ]


bookSummary : List Style
bookSummary =
    [
    ]


book : List Style
book =
    [ flexGrow <| num 1.5
    , position relative
    , width <| rem 0
    , displayFlex
    ]

