module Util exposing (..)

isWhitespace : Char -> Bool
isWhitespace c =
    case c of
        ' '  -> True
        '\t' -> True
        '\n' -> True
        '\r' -> True
        _    -> False
