{- TODO
   section headers; dividers?

   highlight invalid bits in answers

   warnings:
     multiple letters from the same word
     ascending or descending runs

   cleaner puzzle display

   modes/phases
     - Quote Entry
     - Anagramming (quotes uneditable, show puzzle view)
     - Lettering and cluing (anagrams uneditable, too)

   autonumbering (SAT/SMT? CLP (since there may not exist an optimal solution)?)

   answer search
     /usr/share/dict/words
     Wikipedia/Wiktionary titles
     generic JSON API?
     tie in to autocomplete?

   way to control escaped characters in the quote
-}

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)
import Svg
import Svg.Attributes
import Browser


type alias Flags = ()

type Msg =
    Title String
  | Author String
  | Quote String
  | Answer Int String
  | Select Int
  | Hint Int String
  | Number Int Int String

type alias Model = 
    { title : String
    , author : String
    , quote : String
    , clues : List Clue
    , selectedClue : Maybe Int
    }

type alias Clue =
    { hint : String
    , answer : List (Maybe Int, Char)
    }

clueAnswer : Clue -> String
clueAnswer c = c.answer |> List.map Tuple.second |> String.fromList

initialModel =
    { title = ""
    , author = ""
    , quote = ""
    , clues = []
    , selectedClue = Nothing
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
        Title title -> (fixupAnswerInitials { model | title = title }, Cmd.none)
        Author author -> (fixupAnswerInitials { model | author = author }, Cmd.none)
        Quote quote -> ({ model | quote = quote }, Cmd.none)
        Answer idx answer -> ({ model | clues = updateAnswer model idx answer model.clues}, Cmd.none)
        Select idx -> ({ model | selectedClue = 
                             if 0 <= idx && idx < List.length model.clues
                             then Just idx
                             else Nothing }, 
                       Cmd.none)
        Hint idx hint -> ({ model | clues = updateHint idx hint model.clues }, Cmd.none)
        Number idx numIdx newNum -> ({ model | clues = updateNumbering idx numIdx (newNum |> String.toInt) model.clues }, Cmd.none) 

defaultClue : String -> Clue
defaultClue s = { hint = ""
                , answer = s |> String.toList |> List.map (Tuple.pair Nothing)
                }

updateNumbering : Int -> Int -> Maybe Int -> List Clue -> List Clue
updateNumbering index numIndex mQuoteNum clues =
    updateIndex index (\clue -> { clue | answer = updateIndex numIndex (\(_,c) -> (mQuoteNum, c)) clue.answer }) clues

updateHint : Int -> String -> List Clue -> List Clue
updateHint index hint clues =
    case clues of
        [] -> []
        clue::rest ->
            if index == 0
            then { clue | hint = hint }::rest
            else clue::updateHint (index-1) hint rest

updateAnswer : Model -> Int -> String -> List Clue -> List Clue
updateAnswer model index answer clues =
    case clues of
        [] -> [] 
        clue::rest -> 
            if index == 0
            then 
                           
                let
                    
                    numbering = clue.answer |> List.map Tuple.first

                    extendedNumbering = numbering ++ List.repeat (String.length answer - List.length numbering) Nothing

                    numberedAnswer = 
                        answer |> String.toList
                               |> List.map2 
                                    (\mnum c ->
                                         {- FIXME slightly inefficient...  -}
                                         case mnum of
                                             Nothing -> (Nothing, c)
                                             Just num ->
                                                 if quoteIndex model num == Just c
                                                 then (Just num, c)
                                                 else (Nothing, c))
                                    extendedNumbering
                in
                    
                    {clue | answer = numberedAnswer }::rest
                        
            else clue::updateAnswer model (index-1) answer rest

fixupAnswerInitials : Model -> Model
fixupAnswerInitials model =
    let 
        initials = initialism model |> String.toList |> List.map String.fromChar

        clues = 
            {- FIXME with more detailed delta information, we could be smarter here -}
            if List.length model.clues /= List.length initials
            then List.map defaultClue initials
            else List.map2 
                (\i c -> 
                     if String.startsWith i (clueAnswer c)
                     then c
                     else defaultClue i) 
                initials model.clues

    in

        { model | clues = clues }

view : Model -> Html Msg
view model = 
    let initials = initialism model 

        initialismHist = letterHist initials

        quoteHist = letterHist model.quote 
                                     
        missingHist = histDifference quoteHist initialismHist

        viable = isExhaustedHist missingHist

        clueHist = letterHist (model.clues |> List.map clueAnswer |> String.concat)

        remainingHist = histDifference clueHist quoteHist

        quoteIndices =     
            model.quote |> cleanChars
                        |> List.indexedMap (\i c -> (c, i))
                        |> List.foldr (\(c, i) d -> updateCons c i d) Dict.empty

        quoteIndexUses = 
            model.clues |> List.indexedMap (\i clue -> 
                                                List.foldr 
                                                  (\(numIndex, (mNum,_)) d ->
                                                       case mNum of
                                                           Nothing -> d
                                                           Just num -> updateCons num (i, numIndex) d)
                                                  Dict.empty
                                                  (clue.answer |> List.indexedMap Tuple.pair))
                        |> mergeConsMany

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
            , div [id "summary"]
                [ span [id "viability"]
                      [ if viable
                        then text "Quote has all of the initialism's letters"
                        else text ("The quote does not have some letters the initialism needs: " ++ histToShortString missingHist)
                      ]
                , span [class "count"] 
                    [ text "Total letters: "
                    , quoteHist |> countHist |> String.fromInt |> text]
                , span [class "count"] 
                    [ text "Remaining letters: "
                    , remainingHist |> countHist |> String.fromInt |> text]
                ]
            ]
        , section [id "stats"]
            [ histToSVG quoteHist remainingHist ]
        , section [id "clues"]
            (model.clues 
                |> List.map clueAnswer
                |> addInitials (String.toList initials) 
                |> addIndex 
                |> List.map clueEntry)

        , section [id "clue-info"]
            (case model.selectedClue of
                 Nothing -> []
                 Just index -> 
                     let 
                        
                         clueLetter = letterFor index

                         clue = clueFor index model

                         numberingFor numIndex mNum c = 
                             select [id ("clue-numbering-" ++ String.fromInt index ++ "-" ++ String.fromInt numIndex)
                                    , onInput (Number index numIndex)]
                                 ([option [ value ""
                                          , selected (mNum == Nothing)] 
                                          [text "###"]] ++
                                  (List.map
                                       (\qIndex ->
                                            let 

                                                uses = Dict.get qIndex quoteIndexUses 
                                                         |> Maybe.withDefault []
                                                         |> List.filter (\(uIdx, uNumIdx) -> uIdx /= index || (uIdx == index && uNumIdx /= numIndex))

                                                clueMention (cIdx, cNumIdx) = letterFor cIdx ++ ". " ++ (cNumIdx + 1 |> String.fromInt)

                                                useText = if List.isEmpty uses
                                                          then ""
                                                          else " (used by " ++ String.join ", " (List.map clueMention uses) ++ ")"

                                            in

                                            option [ qIndex |> String.fromInt |> value
                                                   , selected (mNum == Just qIndex)
                                                   ] 
                                                   [text ((qIndex + 1 |> String.fromInt) ++ useText)])
                                       (Dict.get c quoteIndices |> Maybe.withDefault [])))

                     in

                         [ h3 [] [ clueLetter ++ ". " |> text ]
                         , textInput [ tabindex (baseTabs + List.length model.clues + 1)
                                     , class "clue-hint"
                                     , value clue.hint
                                     ]
                               "Clue hint text"
                               clue.hint
                               (Hint index)
                         , table [class "clue-numbering"]
                             [ tr []
                                   (List.map 
                                        (\(_, c) ->
                                             td [class "clue-numbering-letter"] 
                                                [c |> String.fromChar |> text])
                                        clue.answer)
                             , tr []
                                   (List.indexedMap
                                        (\numIndex (mNum, rawC) ->
                                             let
                                                 c = Char.toUpper rawC

                                                 validCls = 
                                                     case mNum of
                                                         Nothing -> "unentered"
                                                         Just num ->
                                                             if quoteIndex model num 
                                                                  |> Maybe.map (\qC -> c == Char.toUpper qC)
                                                                  |> Maybe.withDefault False
                                                             then "valid"
                                                             else "invalid"
                                                                                    
                                             in
                                             td [class "clue-numbering-number", class validCls]
                                                [numberingFor numIndex mNum c])
                                        clue.answer)
                             ]
                         ])
        ]

baseTabs : Int
baseTabs = 3 {- title, author, quote -}

clueEntry : (Int, (Char, String)) -> Html Msg
clueEntry (index, (initial, clue)) =
    let 

        initialStr = String.fromChar initial

        letter = letterFor index 
                 
        validCls = class <| if String.startsWith initialStr clue
                            then "valid"
                            else "invalid"

        lbl = "clue-" ++ letter
    in
        div [onClick (Select index)]
            [ label [class "clue-letter", for lbl] [text (letter ++ ". ")]
            , textInput [tabindex (index + baseTabs), name lbl, validCls, 
                         onFocus (Select index)] 
                (initialStr ++ "...") clue (Answer index)
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
letterFor index = List.head (List.drop index lettering) |> Maybe.withDefault ""

clueFor : Int -> Model -> Clue
clueFor index model = 
    List.head (List.drop index model.clues) |> Maybe.withDefault (defaultClue "")


{- Acrostic functions -}

quoteIndex : Model -> Int -> Maybe Char
quoteIndex model index =
    model.quote |> cleanChars
                |> List.drop index
                |> List.head

type alias Hist = Dict Char Int

cleanChars : String -> List Char
cleanChars s =
    s |> String.toUpper 
      |> String.toList
      |> List.filter Char.isAlphaNum         
      {- FIXME doesn't work with diacritics, Greek, etc. -}

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
        emptyHist (cleanChars s)

cleanLetterHist : Hist -> Hist
cleanLetterHist h = Dict.union h emptyLetterHist 

histToShortString : Hist -> String
histToShortString h =
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.toList
      |> List.map (\(c, count) -> c |> String.fromChar |> String.repeat count)
      |> String.concat

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

        hooray =
            if isEmptyHist hr && not (isEmptyHist hq)
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

isExhaustedHist : Hist -> Bool
isExhaustedHist h = 
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.isEmpty
  
isEmptyHist : Hist -> Bool
isEmptyHist h =
    h |> Dict.values
      |> List.all (\cnt -> cnt == 0)

{- utility functions -}

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
