port module Main exposing (..)

{- TODO
   section headers; dividers?

   cleaner puzzle display

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

import Json.Encode
import Json.Decode

import Browser

-- MAIN                    

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

-- MESSAGES, PORTS, FLAGS
    
type alias Flags = Json.Encode.Value {- saved puzzles -}

type Msg =
    Title String
  | Author String
  | Quote String
  | Answer Int String
  | Select Int
  | Hint Int String
  | Number Int Int String
  | Phase Phase
  | Save    

port savePuzzles : Json.Encode.Value -> Cmd msg

-- TYPES, HELPERS                
                   
type Phase = QuoteEntry
           | Anagramming {- quotes uneditable, show puzzle view -}
           | CluingLettering {- answers uneditable, too -}

type alias Clue =
    { hint : String
    , answer : List (Maybe Int, Char)
    }

clueAnswer : Clue -> String
clueAnswer c = c.answer |> List.map Tuple.second |> String.fromList

defaultClue : String -> Clue
defaultClue s = { hint = ""
                , answer = s |> String.toList |> List.map (Tuple.pair Nothing)
                }
               
type alias Puzzle =
    { title : String
    , author : String
    , quote : String
    , clues : List Clue
    , phase : Phase
    }

emptyPuzzle : Puzzle
emptyPuzzle =
    { title = ""
    , author = ""
    , quote = ""
    , clues = []
    , phase = QuoteEntry
    }

-- Puzzle setters
    
setTitle : String -> Puzzle -> Puzzle
setTitle title puzzle = { puzzle | title = title }

setAuthor : String -> Puzzle -> Puzzle
setAuthor author puzzle = { puzzle | author = author }

setQuote : String -> Puzzle -> Puzzle
setQuote quote puzzle = { puzzle | quote = quote }

setPhase : Phase -> Puzzle -> Puzzle
setPhase phase puzzle = { puzzle | phase = phase }
    

updateNumbering : Int -> Int -> Maybe Int -> Puzzle -> Puzzle
updateNumbering index numIndex mQuoteNum puzzle =
    { puzzle | clues =
          updateIndex index
            (\clue ->
                 { clue | answer =
                       updateIndex numIndex (\(_,c) -> (mQuoteNum, c)) clue.answer })
          puzzle.clues
    }

updateHint : Int -> String -> Puzzle -> Puzzle
updateHint index hint puzzle =
    { puzzle | clues =
          updateIndex index
            (\clue -> { clue | hint = hint })
            puzzle.clues
    }

updateAnswer : Int -> String -> Puzzle -> Puzzle
updateAnswer index answer puzzle =    
    { puzzle | clues =
          updateIndex index
          (\clue ->
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
                                                 if quoteIndex puzzle num == Just c
                                                 then (Just num, c)
                                                 else (Nothing, c))
                                    extendedNumbering
                in
                    
                    {clue | answer = numberedAnswer })
            puzzle.clues
    }

fixupAnswerInitials : Puzzle -> Puzzle
fixupAnswerInitials puzzle =
    let 
        initials = initialism puzzle |> String.toList |> List.map String.fromChar

        clues = 
            {- FIXME with more detailed delta information, we could be smarter here -}
            if List.length puzzle.clues /= List.length initials
            then List.map defaultClue initials
            else List.map2 
                (\i c -> 
                     if String.startsWith i (clueAnswer c)
                     then c
                     else defaultClue i) 
                initials puzzle.clues

    in

        { puzzle | clues = clues }

type alias Model = 
    { puzzle : Puzzle
    , selectedClue : Maybe Int
    , savedPuzzles : List Puzzle
    }

asCurrentPuzzleIn : Model -> Puzzle -> Model
asCurrentPuzzleIn model puzzle = { model | puzzle = puzzle }    
    
init : Flags -> (Model, Cmd Msg)
init savedPuzzleJSON =
    ( { puzzle = emptyPuzzle
      , selectedClue = Nothing
      , savedPuzzles = []
      }
    , Cmd.none
    )

textInput : List (Attribute msg) -> String -> String -> (String -> msg) -> Html msg
textInput attrs p v toMsg = 
    input ([ type_ "text", placeholder p, value v, onInput toMsg ] ++ attrs) []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Title title -> (model.puzzle |> setTitle title |> fixupAnswerInitials |> asCurrentPuzzleIn model, Cmd.none)
        Author author -> (model.puzzle |> setAuthor author |> fixupAnswerInitials |> asCurrentPuzzleIn model, Cmd.none)
        Quote quote -> (model.puzzle |> setQuote quote |> fixupAnswerInitials |> asCurrentPuzzleIn model, Cmd.none)
        Answer idx answer -> (model.puzzle |> updateAnswer idx answer |> asCurrentPuzzleIn model, Cmd.none)
        Select idx -> ({ model | selectedClue = 
                             if 0 <= idx && idx < List.length model.puzzle.clues
                             then Just idx
                             else Nothing }, 
                       Cmd.none)
        Hint idx hint -> (model.puzzle |> updateHint idx hint |> asCurrentPuzzleIn model, Cmd.none)
        Number idx numIdx newNum -> (model.puzzle |>  updateNumbering idx numIdx (newNum |> String.toInt) |> asCurrentPuzzleIn model, Cmd.none)
        Phase phase -> (model.puzzle |> setPhase phase |> asCurrentPuzzleIn model, Cmd.none)
        Save -> (model, savePuzzles Json.Encode.null)

view : Model -> Html Msg
view model = 
    let
        puzzle = model.puzzle
        
        quoteFixed = puzzle.phase /= QuoteEntry

        answersFixed = puzzle.phase == CluingLettering

        initials = initialism puzzle 

        initialismHist = letterHist initials

        quoteHist = letterHist puzzle.quote 
                                     
        missingHist = histDifference quoteHist initialismHist

        viable = isExhaustedHist missingHist

        clueHist = letterHist (puzzle.clues |> List.map clueAnswer |> String.concat)

        remainingHist = histDifference clueHist quoteHist

        readyForPhase phase =
            puzzle.phase == phase ||
                case phase of
                    QuoteEntry -> True
                    Anagramming -> viable &&
                                   not (isEmptyHist quoteHist)
                    CluingLettering -> viable &&
                                       not (isEmptyHist quoteHist) &&
                                       isEmptyHist remainingHist
                        
        quoteIndices =     
            puzzle.quote |> cleanChars
                        |> List.indexedMap (\i c -> (c, i))
                        |> List.foldr (\(c, i) d -> updateCons c i d) Dict.empty

        quoteIndexWords =
            puzzle.quote |> String.words
                        |> List.map cleanChars
                        |> List.filter (not << List.isEmpty)
                        |> List.indexedMap (\i w -> List.repeat (List.length w) i)
                        |> List.concat
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                           
        quoteIndexUses = 
            puzzle.clues |> List.indexedMap (\i clue -> 
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
        [ section [id "overview"]
              [ h1 [] [text "Crossbars — Acrostic Constructor"]
              , div [] (List.intersperse (span [] [text " → "])
                            (List.map
                                 (\p ->
                                      input [ type_ "button"
                                            , class "phase"
                                            , class (if p == puzzle.phase then "active" else "inactive")
                                            , disabled (not (readyForPhase p))
                                            , value (stringOfPhase p)
                                            , onClick (Phase p)
                                            ]
                                            [])
                                 phases))
              ]
        , section [id "quote"]
            [ textInput [tabindex 1, size 60, disabled quoteFixed]
                  "Title" puzzle.title Title
            , textInput [tabindex 2, size 60, disabled quoteFixed]
                  "Author" puzzle.author Author
            , textarea [ tabindex 3 {- see baseTabs below -}
                       , disabled quoteFixed
                       , placeholder "Quote"
                       , onInput Quote
                       , rows 6
                       , cols 60
                       , attribute "autocapitalize" "character"
                       ] 
                  [text puzzle.quote]
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
            (puzzle.clues 
                |> List.map clueAnswer
                |> addInitials (String.toList initials) 
                |> addIndex 
                |> List.map (clueEntry answersFixed))

        , section [id "clue-info"]
            (case model.selectedClue of
                 Nothing -> []
                 Just index -> 
                     let 
                        
                         clueLetter = letterFor index

                         clue = clueFor index puzzle

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

                                                useText =
                                                    if List.isEmpty uses
                                                    then ""
                                                    else " (used by " ++ String.join ", " (List.map clueMention uses) ++ ")"

                                            in

                                            option [ qIndex |> String.fromInt |> value
                                                   , selected (mNum == Just qIndex)
                                                   ] 
                                                   [text ((qIndex + 1 |> String.fromInt) ++ useText)])
                                       (Dict.get c quoteIndices |> Maybe.withDefault [])))

                         clueNumbers = clue.answer |> List.indexedMap Tuple.pair
                                                   |> List.filterMap (\(ansIndex,(mNumIndex,_)) -> Maybe.map (Tuple.pair ansIndex) mNumIndex)

                         unindexedClueNumbers = clueNumbers |> List.map Tuple.second
                 
                         fullyNumbered = List.map (Tuple.second >> Just) clueNumbers == List.map Tuple.first clue.answer

                         clueWords = List.filterMap
                                       (\(ansIndex, numIndex) ->
                                              Dict.get numIndex quoteIndexWords |> Maybe.map (Tuple.pair ansIndex))
                                       clueNumbers

                         dupWords = List.filter
                                      (\(ansIndex, wIndex) ->
                                           List.any
                                             (\(otherAnsIndex, otherWIndex) -> ansIndex /= otherAnsIndex && wIndex == otherWIndex)
                                                 clueWords)
                                             clueWords

                         dupLetters = List.map Tuple.first dupWords

                     in

                         [ h3 [] [ clueLetter ++ ". " |> text ]
                         , textInput [ tabindex (baseTabs + List.length puzzle.clues + 1)
                                     , class "clue-hint"
                                     , value clue.hint
                                     ]
                               "Clue hint text"
                               clue.hint
                               (Hint index)
                         , table [class "clue-numbering"]
                             [ tr []
                                   (List.indexedMap 
                                        (\numIndex (_, c) ->
                                             let

                                                 dupClasses =
                                                     if List.member numIndex dupLetters
                                                     then [ class "double-dipped" ]
                                                     else []
                                             
                                             in
                                                 td ([class "clue-numbering-letter"] ++ dupClasses)
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
                                                             if quoteIndex puzzle num 
                                                                  |> Maybe.map (\qC -> c == Char.toUpper qC)
                                                                  |> Maybe.withDefault False
                                                             then "valid"
                                                             else "invalid"
                                                                                    
                                             in
                                             td [class "clue-numbering-number", class validCls]
                                                [numberingFor numIndex mNum c])
                                        clue.answer)
                             ]
                         , let

                               editableWarning = if puzzle.phase /= CluingLettering
                                                 then Just (span [] [text "Editing the clue will erase any numbers you have entered."])
                                                 else Nothing
                               
                               ascendingWarning = if fullyNumbered && List.sort unindexedClueNumbers == unindexedClueNumbers
                                                  then Just (span [] [text "Clue numbers are an ascending run."])
                                                  else Nothing

                               descendingWarning = if fullyNumbered && List.sort unindexedClueNumbers == List.reverse unindexedClueNumbers
                                                   then Just (span [] [text "Clue numbers are a descending run."])
                                                   else Nothing

                               duplicateWarning = if not (List.isEmpty dupWords)
                                                  then Just (span [] [ text "Highlighted clue letters come from the same word." ])
                                                  else Nothing
                                                       
                               warnings = [editableWarning, ascendingWarning, descendingWarning, duplicateWarning]

                           in
                               div [class "warnings"] (List.filterMap identity warnings)
                         ])
        ]

baseTabs : Int
baseTabs = 3 {- title, author, quote -}

stringOfPhase : Phase -> String
stringOfPhase p =
    case p of
        QuoteEntry -> "Quote entry"
        Anagramming -> "Anagramming"
        CluingLettering -> "Cluing and lettering"

phases : List Phase
phases = [QuoteEntry, Anagramming, CluingLettering]
         
clueEntry : Bool -> (Int, (Char, String)) -> Html Msg
clueEntry answersFixed (index, (initial, clue)) =
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
                         onFocus (Select index), disabled answersFixed] 
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

clueFor : Int -> Puzzle -> Clue
clueFor index puzzle = 
    List.head (List.drop index puzzle.clues) |> Maybe.withDefault (defaultClue "")

{- Acrostic functions -}

quoteIndex : Puzzle -> Int -> Maybe Char
quoteIndex puzzle index =
    puzzle.quote |> cleanChars
                 |> List.drop index
                 |> List.head

type alias Hist = Dict Char Int

cleanChars : String -> List Char
cleanChars s =
    s |> String.toUpper 
      |> String.toList
      |> List.filter Char.isAlphaNum         
      {- FIXME doesn't work with diacritics, Greek, etc. -}

initialism : Puzzle -> String
initialism puzzle = puzzle.author ++ puzzle.title |> String.filter Char.isAlphaNum

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
