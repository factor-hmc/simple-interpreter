module Builtins exposing (..)

import List (singleton)
import Dict exposing (..)
import Lang exposing (..)

--
--builtins : Dict String (List Word)
--builtins = fromList <| map (\(str, b) -> (str, singleton (Builtin b)))
--           [ ("+", Add)
--           , ("-", Sub)
--           , ("*", Mul)
--           , ("/", Div)
--           , ("=", Eq)
--           , ("if", If)
--           , ("dup", Dup)
--           , ("swap", Swap)
--           , ("rot", Rot)
--           , ("drop", Drop)
--           , ("while", While)
--           , ("clear", Clear)
--           , ("call", Call)
--           ]
