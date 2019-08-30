module Hist exposing 
    ( Hist
    , empty
    , fromString
    , foundIn
    , difference
    , isEmpty
    , isExhausted
    , count
    , toShortString
    , toSVG
    )

import Dict exposing (Dict)
import Html exposing (Html)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Util exposing (..)

type alias Hist = Dict Char Int
         
empty : Hist
empty = Dict.empty

emptyLetter : Hist
emptyLetter = 
    alphabetList
        |> List.map (\c -> (c,0))
        |> Dict.fromList

fromString : String -> Hist
fromString s =
    let 
        incrementCount mcnt =
            case mcnt of
             Nothing -> Just 1
             Just cnt -> Just (cnt + 1)
    in

    List.foldl (\c m -> Dict.update c incrementCount m)
        empty (cleanChars s)

cleanLetterHist : Hist -> Hist
cleanLetterHist h = Dict.union h emptyLetter

count : Hist -> Int
count h = List.sum (Dict.values h)

foundIn : Hist -> String -> Bool
foundIn hist s = 
    s |>
    fromString |>
    difference hist |>
    isExhausted

difference : Hist -> Hist -> Hist
difference hSub hSup = 
    Dict.merge
        (\c cSub      d -> Dict.insert c (-cSub) d)
        (\c cSub cSup d -> Dict.insert c (cSup - cSub) d)
        (\c      cSup d -> Dict.insert c cSup d)
        hSub hSup
        Dict.empty

isExhausted : Hist -> Bool
isExhausted h = 
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.isEmpty
  
isEmpty : Hist -> Bool
isEmpty h =
    h |> Dict.values
      |> List.all (\cnt -> cnt == 0)

-- HISTOGRAM RENDERING
         
toShortString : Hist -> String
toShortString h =
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.toList
      |> List.map (\(c, cnt) -> c |> String.fromChar |> String.repeat cnt)
      |> String.concat

toSVG : Hist -> Hist -> Html msg
toSVG hQuote hRemaining =
    let 
        width = 300
        height = 120

        hq = hQuote |> cleanLetterHist 
        hr = hRemaining |> cleanLetterHist 
        hc = Dict.union hq hr

        allLetters = Dict.keys hc
        letterSpacing = width / toFloat (List.length allLetters)
        letterStart = letterSpacing / 2
        letterX index = letterStart + letterSpacing * toFloat index
        letterY = height - 2
        letterLabels = 
            List.indexedMap
                (\index letter ->
                     Svg.text_ 
                         [ letterX index |> String.fromFloat |> Svg.Attributes.x
                         , letterY |> String.fromFloat |> Svg.Attributes.y
                         , Svg.Attributes.textAnchor "middle"
                         , Svg.Attributes.class "label"
                         ]
                         [ letter |> String.fromChar |> Svg.text ])
                allLetters

        maxRemaining = hc |> Dict.values 
                          |> List.maximum |> Maybe.withDefault 0 |> toFloat
        barHeight cnt = 100 * (toFloat cnt / maxRemaining)
        barWidth = letterSpacing * 0.75
        barX index = letterX index - (barWidth / 2)
        barBaseY = height - 10

        bars cls h =             
            if maxRemaining > 0
            then List.indexedMap 
                  (\index cnt ->
                       let             
                           barH = barHeight cnt
                           barY = Basics.min (barBaseY - barH) barBaseY
                           bar = Svg.rect
                                 [ barX index |> String.fromFloat |> Svg.Attributes.x 
                                 , barY |> String.fromFloat |> Svg.Attributes.y
                                 , barWidth |> String.fromFloat |> Svg.Attributes.width
                                 , barH |> String.fromFloat |> Svg.Attributes.height
                                 ] []

                           countCls = if cnt >= 0 then "valid" else "invalid"
                           counter = Svg.text_
                                   [ letterX index |> String.fromFloat |> Svg.Attributes.x
                                   , barY - 2 |> String.fromFloat |> Svg.Attributes.y
                                   , Svg.Attributes.textAnchor "middle"
                                   , Svg.Attributes.class countCls
                                   ]
                                   [ cnt |> String.fromInt |> Svg.text ]

                       in
                       Svg.g [Svg.Attributes.class cls] [bar, counter])
                  (Dict.values h)
            else []

        quoteBars = bars "quote" hq
        remainingBars = bars "remaining" hr

        hooray =
            if isEmpty hr && not (isEmpty hq)
            then [ Svg.text_
                       [ width / 2 |> String.fromFloat |> Svg.Attributes.x
                       , height / 2 |> String.fromFloat |> Svg.Attributes.y
                       , Svg.Attributes.textAnchor "middle"
                       , Svg.Attributes.dominantBaseline "middle"
                       , Svg.Attributes.class "hooray"
                       ]
                       [ Svg.text "🎉" ]
                 ]
            else []
    in

        Svg.svg 
            [ Svg.Attributes.viewBox 
                  ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
            , id "remaining" 
            ]
            (letterLabels ++
             quoteBars ++
             remainingBars ++
             hooray ++
             [ Svg.title [] [Svg.text "Letters remaining"]
             ])
