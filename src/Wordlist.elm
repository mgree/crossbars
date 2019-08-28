module Wordlist exposing (..)

import Dict exposing (Dict)

import Histogram exposing (..)
import Puzzle exposing (..)

{- FIXME need a prefix tree or something -}
type alias Wordlist = Dict Char (List String)

anagramsFor : Wordlist -> Hist -> List Char -> List String
anagramsFor wl remainingHist prefix =
    case prefix of 
        [] -> []
        c::rest ->
            let s = String.fromList prefix in
            Dict.get c wl |>
            Maybe.withDefault [] |>
            List.filter (String.startsWith s) |>
            List.filter (String.dropLeft (String.length s) >> 
                         foundInHist remainingHist)

testingWordlist : Wordlist
testingWordlist = Dict.fromList <|
    [ ('A', ["ABC", "ABCD", "ABF", "ABD", "ACF", "AFC"])
    , ('B', ["BCD", "BFD", "BDF", "BFF"])
    , ('C', [])
    , ('D', [])
    , ('E', [])
    , ('F', [])
    , ('G', [])
    , ('H', [])
    , ('I', [])
    , ('J', [])
    , ('K', [])
    , ('L', [])
    , ('M', [])
    , ('N', [])
    , ('O', [])
    , ('P', [])
    , ('Q', [])
    , ('R', [])
    , ('S', [])
    , ('T', [])
    , ('U', [])
    , ('V', [])
    , ('W', [])
    , ('X', [])
    , ('Y', [])
    , ('Z', [])
    ]

