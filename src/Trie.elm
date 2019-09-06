module Trie exposing
  ( Trie
  , Entry
  , compareEntry
  , empty
  , insert
  , lookup
  , suffixes
  )

import Dict exposing (Dict)

import Util exposing (..)

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

empty : Trie
empty = Dict.empty

insert : Entry -> Trie -> Trie
insert entry t1 =
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

lookup : String -> Trie -> Maybe Entry
lookup word t1 =
    case word |> String.toList of
        c1::c2::c3::_ ->
            Dict.get c1 t1 |> 
            Maybe.andThen (Dict.get c2) |>
            Maybe.andThen (Dict.get c3) |>
            Maybe.andThen (List.filter (\entry -> entry.word == word) >> List.head)
        _ -> Nothing

suffixes : List Char -> String -> Trie -> List Entry
suffixes word s t1 =
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
        [_] -> []
        [] -> []
