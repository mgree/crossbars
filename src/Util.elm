module Util exposing (..)

import Dict exposing (Dict)

import Json.Encode
import Json.Decode

import Time

alphabet : String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 

alphabetList : List Char
alphabetList = alphabet |> String.toList

cleanString : String -> String
cleanString s =
    s |> String.toUpper 
      |> String.filter Char.isAlphaNum         
      {- FIXME doesn't work with diacritics, Greek, etc. -}

cleanChars : String -> List Char
cleanChars s = s |> cleanString |> String.toList

encodeNullable : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeNullable encode ma =
    case ma of
        Nothing -> Json.Encode.null
        Just a -> encode a

updateIndex : Int -> (a -> a) -> List a -> List a
updateIndex index f l =
    case l of
        [] -> []
        x::rest -> 
            if index == 0
            then f x::rest
            else x::updateIndex (index - 1) f rest

updateCons : comparable -> v -> Dict comparable (List v) -> Dict comparable (List v)
updateCons k v d =
    Dict.update k
        (\mvs ->
             case mvs of
                 Nothing -> Just [v]
                 Just vs -> Just (v::vs))
        d

updateAppend : comparable -> List v -> Dict comparable (List v) -> Dict comparable (List v)
updateAppend k v d =
    Dict.update k
        (\mvs ->
             case mvs of
                 Nothing -> Just v
                 Just vs -> Just (v ++ vs))
        d

mergeCons : Dict comparable (List v) -> Dict comparable (List v) -> Dict comparable (List v)
mergeCons d1 d2 = Dict.foldr (\k vs d -> updateAppend k vs d) d2 d1

mergeConsMany : List (Dict comparable (List v)) -> Dict comparable (List v)
mergeConsMany l = 
    case l of
        [] -> Dict.empty
        [d] -> d
        d::ds -> mergeCons d (mergeConsMany ds)

insertWith : (a -> a -> Order) -> a -> List a -> List a
insertWith cmp x l =
    case l of
        [] -> [x]
        y::rest ->
            case cmp x y of
                LT -> x::y::rest
                EQ -> x::y::rest
                GT -> y::insertWith cmp x rest

allPairs : List a -> List (a,a)
allPairs l =
    case l of
        [] -> []
        hd::tl -> List.map (Tuple.pair hd) tl ++ allPairs tl

listInsert : a -> List a -> List a
listInsert x l = if List.member x l then l else x::l

type alias SplitList = 
    { three      : List String
    , four       : List String
    , five       : List String
    , six        : List String
    , sevenPlus  : List String
    }

emptySplitList = 
    { three = []
    , four = []
    , five = []
    , six = []
    , sevenPlus = []
    }

splitList : List String -> SplitList
splitList list =
    let loop l sl =
            case l of
                [] -> sl
                s::rest -> 
                    case String.length s of
                        0 -> loop rest sl
                        1 -> loop rest sl
                        2 -> loop rest sl
                        3 -> loop rest { sl | three = s::sl.three }
                        4 -> loop rest { sl | four = s::sl.four }
                        5 -> loop rest { sl | five = s::sl.five }
                        6 -> loop rest { sl | six = s::sl.six }
                        _ -> loop rest { sl | sevenPlus = s::sl.sevenPlus }
    in
        loop (List.reverse list) emptySplitList
                     
iso8601DateTime : Time.Zone -> Time.Posix -> String
iso8601DateTime here now =
    iso8601Date here now ++ " " ++ iso8601Time here now

iso8601Date : Time.Zone -> Time.Posix -> String
iso8601Date here now =
    let yyyy = Time.toYear here now |>
               String.fromInt
        mm   = case Time.toMonth here now of
                   Time.Jan -> "01"
                   Time.Feb -> "02"
                   Time.Mar -> "03"
                   Time.Apr -> "04"
                   Time.May -> "05"
                   Time.Jun -> "06"
                   Time.Jul -> "07"
                   Time.Aug -> "08"
                   Time.Sep -> "09"
                   Time.Oct -> "10"
                   Time.Nov -> "11"
                   Time.Dec -> "12"
        dd   = Time.toDay here now |> twoDigits
    in
        String.join "-" [yyyy, mm, dd]

iso8601Time : Time.Zone -> Time.Posix -> String
iso8601Time here now =
    let hh = Time.toHour here now |> twoDigits
        mm = Time.toMinute here now |> twoDigits
        ss = Time.toSecond here now |> twoDigits
    in
        String.join ":" [hh, mm, ss]

twoDigits : Int -> String 
twoDigits i = i |>
              String.fromInt |>
              String.padLeft 2 '0'
