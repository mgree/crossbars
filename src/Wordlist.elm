module Wordlist exposing (..)

import Dict exposing (Dict)

import Histogram exposing (..)
import Puzzle exposing (..)
import Util exposing (..)

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

-- fixed depth 3 tries

type alias Trie = Dict Char (Dict Char (Dict Char (List Entry)))

type alias Entry = 
    { word : String
    , source : String
    , desc : String
    , url : String
    }

compareEntry : Entry -> Entry -> Order
compareEntry e1 e2 = compare e1.word e2.word

emptyTrie : Trie
emptyTrie = Dict.empty

trieInsert : Entry -> Trie -> Trie
trieInsert entry t1 =
    case entry.word |> String.toList of
        c1::c2::c3::_ ->
            let t2 = Dict.get c1 t1 |>
                     Maybe.withDefault Dict.empty

                t2Updated = 
                    let t3 = Dict.get c2 t2 |>
                             Maybe.withDefault Dict.empty

                        t3Updated =
                            Dict.update c3
                                (\mstrs ->
                                     mstrs |> 
                                     Maybe.withDefault [] |>
                                     insertWith compareEntry entry |>
                                     Just)
                                t3
                    in
                        Dict.insert c2 t3Updated t2
 
            in
                Dict.insert c1 t2Updated t1

        _ -> t1

trieLookup : String -> Trie -> Maybe Entry
trieLookup word t1 =
    case word |> String.toList of
        c1::c2::c3::_ ->
            Dict.get c1 t1 |> 
            Maybe.andThen (Dict.get c2) |>
            Maybe.andThen (Dict.get c3) |>
            Maybe.andThen (List.filter (\entry -> entry.word == word) >> List.head)
        _ -> Nothing

generateWordlist : List String -> Trie
generateWordlist words =
    words |>
    List.map 
        (\word ->
             { word = word
             , source = ""
             , desc = ""
             , url = ""
             }) |>
    List.foldr trieInsert emptyTrie

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

