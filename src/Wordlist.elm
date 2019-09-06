module Wordlist exposing 
    ( Wordlist
    , empty
    , anagramsFor
    , load
    , fromSourceStrings
    )

import Dict exposing (Dict)

import Hist exposing (Hist)
import Puzzle exposing (Puzzle)
import Trie exposing (Trie)
import Util exposing (..)

import Parser exposing (Parser, (|.), (|=), symbol, end, succeed, spaces, chompUntilEndOr, getChompedString)

type alias Wordlist = Trie

empty : Wordlist
empty = Trie.empty

anagramsFor : Wordlist -> Hist -> List Char -> List String
anagramsFor wl remainingHist prefix =
    case prefix of 
        [] -> []
        c::rest ->
            let s = String.fromList prefix in
            Trie.suffixes prefix s wl |>
            List.map .word |>
            List.filter (String.dropLeft (String.length s) >> 
                         Hist.foundIn remainingHist)

-- parsing word lists

load : String -> String -> Wordlist
load source contents =
    contents |>
    String.words |>
    fromSourceStrings source

fromSourceStrings : String -> List String -> Wordlist
fromSourceStrings source words =
    words |>
    List.map 
        (\word ->
             { word = String.toUpper word
             , source = source
             , desc = ""
             , url = ""
             }) |>
    List.foldr Trie.insert Trie.empty
