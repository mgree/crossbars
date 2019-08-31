module Wordlist exposing (..)

import Dict exposing (Dict)

import Hist exposing (..)
import Puzzle exposing (..)
import Util exposing (..)

import Parser exposing (Parser, (|.), (|=), symbol, end, succeed, spaces, chompUntilEndOr, getChompedString)

type alias Wordlist = Trie

empty : Wordlist
empty = emptyTrie

anagramsFor : Wordlist -> Hist -> List Char -> List String
anagramsFor wl remainingHist prefix =
    case prefix of 
        [] -> []
        c::rest ->
            let s = String.fromList prefix in
            trieSuffixes prefix s wl |>
            List.map .word |>
            List.filter (String.dropLeft (String.length s) >> 
                         Hist.foundIn remainingHist)

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

trieSuffixes : List Char -> String -> Trie -> List Entry
trieSuffixes word s t1 =
    case word of
        c1::c2::c3::_ ->
            Dict.get c1 t1 |> 
            Maybe.andThen (Dict.get c2) |>
            Maybe.andThen (Dict.get c3) |>
            Maybe.withDefault [] |>
            List.filter (.word >> String.startsWith s)
        [c1, c2] ->
            Dict.get c1 t1 |> 
            Maybe.andThen (Dict.get c2) |>
            Maybe.withDefault Dict.empty |>
            Dict.values |>
            List.concat
        [c1] ->
            Dict.get c1 t1 |> 
            Maybe.withDefault Dict.empty |>
            Dict.values |>
            List.concatMap Dict.values |>
            List.concat
        [] -> []

generateWordlist : String -> List String -> Trie
generateWordlist source words =
    words |>
    List.map 
        (\word ->
             { word = word
             , source = source
             , desc = ""
             , url = ""
             }) |>
    List.foldr trieInsert emptyTrie

parseWordPerLine : Parser (List String)
parseWordPerLine =
    Parser.oneOf
        [ succeed (\hd tl -> if String.isEmpty hd then tl else hd :: tl)
            |= (chompUntilEndOr "\n" |> getChompedString)
            |= Parser.oneOf
              [ succeed identity
                  |. symbol "\n"
                  |= Parser.lazy (\_ -> parseWordPerLine)
              , succeed []
                |. end
              ]
        , succeed []
            |. end
        ]

