{- TODO
   highlight invalid bits clues

   use CSS flex to wrap columns in clues

   cleaner puzzle display

   modes/phases
     - Quote Entry
     - Anagramming (quotes uneditable, show puzzle view)
     - Lettering and cluing (anagrams uneditable, too)

   autonumbering (SAT/SMT? CLP (since there may not exist an optimal solution)?)

   way to control escaped characters in the quote
-}

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg
import Svg.Attributes
import Browser


type alias Flags = ()

type Msg =
    Title String
  | Author String
  | Quote String
  | Clue Int String

type alias Model = 
    { title : String
    , author : String
    , quote : String
    , clues : List String
    }

initialModel =
    { title = ""
    , author = ""
    , quote = ""
    , clues = []
    }

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

init : Flags -> (Model, Cmd Msg)
init flags = (initialModel, Cmd.none)

textInput : List (Attribute msg) -> String -> String -> (String -> msg) -> Html msg
textInput attrs p v toMsg = 
    input ([ type_ "text", placeholder p, value v, onInput toMsg ] ++ attrs) []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Title title -> (updateClues { model | title = String.toUpper title }, Cmd.none)
        Author author -> (updateClues { model | author = String.toUpper author }, Cmd.none)
        Quote quote -> ({ model | quote = String.toUpper quote }, Cmd.none)
        Clue idx clue -> ({ model | clues = updateIndex idx (String.toUpper clue) model.clues}, Cmd.none)

updateClues : Model -> Model
updateClues model =
    { model | clues = initialism model |> String.toList |> List.map String.fromChar }

view : Model -> Html Msg
view model = 
    let initials = initialism model 

        initialismHist = letterHist initials

        quoteHist = letterHist model.quote 
                                     
        missingHist = histDifference quoteHist initialismHist

        viable = isEmptyHist missingHist

        clueHist = letterHist (String.concat model.clues)

        remainingHist = histDifference clueHist quoteHist
    in

    div [id "crossbars-wrapper"] 
        [ section [id "quote"]
            [ textInput [tabindex 1, size 60] "Title" model.title Title
            , textInput [tabindex 2, size 60] "Author" model.author Author
            , textarea [ tabindex 3 {- see baseTabs below -}
                       , placeholder "Quote"
                       , onInput Quote
                       , rows 6
                       , cols 60
                       , attribute "autocapitalize" "character"
                       ] 
                  [text model.quote]
            , span []
                [ if viable
                  then text "Viable acrostic"
                  else text ("Non-viable acrostic; the quote does not have some letters the initialism needs: " ++ histToShortString missingHist)
                ]
            , span [] [ text "Total letters: "
                      , quoteHist |> countHist |> String.fromInt |> text]
            , span [] [ text "Remaining letters: "
                      , remainingHist |> countHist |> String.fromInt |> text]
            ]
        , section [id "stats"]
            [ histToSVG quoteHist remainingHist ]
        , section [id "clues"]
            [ viewClues (String.toList initials) model.clues ]
        ]

baseTabs : Int
baseTabs = 3 {- title, author, quote -}

viewClues : List Char -> List String -> Html Msg
viewClues initials clues = 
    let clueRows = clues |> addInitials initials |> addIndex |> 
                   breakSublists 5 |> transpose in
    table 
      [id "clues"]
      (List.map (\clueRow -> tr [] (List.concatMap clueEntry clueRow)) clueRows)

clueEntry : (Int, (Char, String)) -> List (Html Msg)
clueEntry (index, (initial, clue)) =
    let 

        initialStr = String.fromChar initial

        letter = letterFor index 
                 
        valid = String.startsWith initialStr clue

        lbl = "clue-" ++ letter
    in

        [ td [class "clue-letter"] 
              [label [for lbl] [text (letter ++ ". ")]]
        , textInput [tabindex (index + baseTabs), name lbl] 
           ("Clue starting with " ++ initialStr) clue (Clue index)
        ] 

transpose : List (List a) -> List (List a)
transpose ls =
    if List.isEmpty ls
    then []
    else (List.filterMap List.head ls) :: transpose (List.filterMap List.tail ls)

breakSublists : Int -> List a -> List (List a)
breakSublists len l =
    if List.isEmpty l
    then []
    else List.take len l :: breakSublists len (List.drop len l)

addInitials : List Char -> List String -> List (Char, String)
addInitials initial clues = List.map2 Tuple.pair initial clues

addIndex : List a -> List (Int, a)
addIndex l = List.indexedMap Tuple.pair l

lettering : List String
lettering =
    let 
        aToZ = List.map String.fromChar alphabetList
        aaToZZ = List.map (String.repeat 2) aToZ 
    in
        aToZ ++ aaToZZ

letterFor : Int -> String
letterFor index = 
    case List.head (List.drop index lettering) of
        Nothing -> ""
        Just letter -> letter

{- Acrostic functions -}

type alias Hist = Dict Char Int

histogramChars : String -> List Char
histogramChars s =
    s |> String.toUpper 
      |> String.toList
      |> List.filter Char.isAlphaNum

initialism : Model -> String
initialism model = model.author ++ model.title |> String.filter Char.isAlphaNum

emptyHist : Hist
emptyHist = Dict.empty

alphabet : String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 

alphabetList : List Char
alphabetList = alphabet |> String.toList

emptyLetterHist : Hist
emptyLetterHist = 
    alphabetList
        |> List.map (\c -> (c,0))
        |> Dict.fromList

letterHist : String -> Hist
letterHist s =
    let 
        incrementCount mcnt =
            case mcnt of
             Nothing -> Just 1
             Just cnt -> Just (cnt + 1)
    in

    List.foldl (\c m -> Dict.update c incrementCount m)
        emptyHist (histogramChars s)

cleanLetterHist : Hist -> Hist
cleanLetterHist h =
    Dict.union h emptyLetterHist 
{-        |> Dict.filter (\c cnt -> cnt >= 0) -}

histToShortString : Hist -> String
histToShortString h =
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.toList
      |> List.map (\(c, count) -> c |> String.fromChar |> String.repeat count)
      |> String.concat

histToHtml : String -> Hist -> Html msg
histToHtml histId h =
    let hl = h |> cleanLetterHist |> Dict.toList in
    table [class "histogram", id histId]
        [ tr [] (List.map (\(c,cnt) -> histEntryTD cnt (text (String.fromChar c))) hl)
        , tr [] (List.map (\(c,cnt) -> histEntryTD cnt (text (String.fromInt cnt))) hl)
        ]

histToSVG : Hist -> Hist -> Html msg
histToSVG hQuote hRemaining =
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
                           count = Svg.text_
                                 [ letterX index |> String.fromFloat |> Svg.Attributes.x
                                 , barY - 2 |> String.fromFloat |> Svg.Attributes.y
                                 , Svg.Attributes.textAnchor "middle"
                                 , Svg.Attributes.class countCls
                                 ]
                                 [ cnt |> String.fromInt |> Svg.text ]

                       in
                       Svg.g [Svg.Attributes.class cls] [bar, count])
                  (Dict.values h)
            else []

        quoteBars = bars "quote" hq
        remainingBars = bars "remaining" hr
    in

        Svg.svg 
            [ Svg.Attributes.viewBox 
                  ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
            , id "remaining" 
            ]
            (letterLabels ++
             quoteBars ++
             remainingBars ++
             [ Svg.title [] [Svg.text "Letters remaining"]
             ])
    

histEntryTD : Int -> Html msg -> Html msg
histEntryTD cnt inner = 
    td (if cnt <= 0 then [class "exhausted"] else []) [inner]

countHist : Hist -> Int
countHist h = List.sum (Dict.values h)

histDifference : Hist -> Hist -> Hist
histDifference hSub hSup = 
    Dict.merge
        (\c cSub      d -> Dict.insert c (-cSub) d)
        (\c cSub cSup d -> Dict.insert c (cSup - cSub) d)
        (\c      cSup d -> Dict.insert c cSup d)
        hSub hSup
        Dict.empty

isEmptyHist : Hist -> Bool
isEmptyHist h = 
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.isEmpty
  
updateIndex : Int -> a -> List a -> List a
updateIndex n v l =
    case l of
        [] -> [v]
        h::t -> if n == 0
                then v::t
                else h::updateIndex (n-1) v t
