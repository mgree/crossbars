port module Main exposing (..)

{- TODO

   different ways to sort saved puzzles

   way to delete puzzles

   prompt to save when no title is given?

   section headers; dividers?

   autonumbering (SAT/SMT? CLP (since there may not exist an optimal solution)?)

   answer search
     /usr/share/dict/words
     Wikipedia/Wiktionary titles
     generic JSON API?
     tie in to autocomplete?

   way to control escaped characters in the quote
-}

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)
import Html.Keyed as Keyed

import Svg
import Svg.Attributes

import Json.Encode
import Json.Decode

import Parser exposing (Parser, (|.), (|=), symbol, end, succeed, spaces)

import Task

import Time

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
  | SelectClue Int
  | SelectClues (List Int)
  | Hint Int String
  | Number Int Int String
  | Phase Phase
  | Save Time.Posix
  | NewPuzzle
  | Load Puzzle
  | ApplyNumbering SMTNumbering

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
    , timeModified : Time.Posix
    }

emptyPuzzle :  Puzzle
emptyPuzzle =
    { title = ""
    , author = ""
    , quote = ""
    , clues = []
    , phase = QuoteEntry
    , timeModified = Time.millisToPosix 0
    }

puzzleDescription : Puzzle -> String
puzzleDescription puzzle =
    {- FIXME need Time.here to render modified time -}
    String.toUpper puzzle.author ++ " — " ++ String.toUpper puzzle.title
    
comparePuzzles : Puzzle -> Puzzle -> Order
comparePuzzles puz1 puz2 =
    compare (Time.posixToMillis puz1.timeModified) (Time.posixToMillis puz2.timeModified)

samePuzzle : Puzzle -> Puzzle -> Bool
samePuzzle puz1 puz2 = comparePuzzles puz1 puz2 == EQ

-- Puzzle setters
    
setTitle : String -> Puzzle -> Puzzle
setTitle title puzzle = { puzzle | title = title }

setAuthor : String -> Puzzle -> Puzzle
setAuthor author puzzle = { puzzle | author = author }

setQuote : String -> Puzzle -> Puzzle
setQuote quote puzzle = { puzzle | quote = quote }

setPhase : Phase -> Puzzle -> Puzzle
setPhase phase puzzle = { puzzle | phase = phase }

setTimeModified : Time.Posix -> Puzzle -> Puzzle
setTimeModified now puzzle = { puzzle | timeModified = now }
                        
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
    , selectedClues : List Int
    , savedPuzzles : List Puzzle
    }

emptyModel : Model
emptyModel =
    { puzzle = emptyPuzzle
    , selectedClues = []
    , savedPuzzles = []
    }
    
asCurrentPuzzleIn : Model -> Puzzle -> Model
asCurrentPuzzleIn model puzzle = { model | puzzle = puzzle }    

-- PUZZLE SAVING

encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ("puzzle", encodePuzzle model.puzzle)
        , ("selectedClues", Json.Encode.list Json.Encode.int model.selectedClues)
        , ("savedPuzzles", Json.Encode.list encodePuzzle model.savedPuzzles)
        ]
              
encodePuzzle : Puzzle -> Json.Encode.Value
encodePuzzle puzzle =
    Json.Encode.object
        [ ("title", Json.Encode.string puzzle.title)
        , ("author", Json.Encode.string puzzle.author)
        , ("quote", Json.Encode.string puzzle.quote)
        , ("clues", Json.Encode.list encodeClue puzzle.clues)
        , ("phase", encodePhase puzzle.phase)
        , ("timeModified", Json.Encode.int <| Time.posixToMillis <| puzzle.timeModified)
        ]

encodeClue : Clue -> Json.Encode.Value
encodeClue clue =
    Json.Encode.object
        [ ("hint", Json.Encode.string clue.hint)
        , ("answer", Json.Encode.list encodeAnswer clue.answer)
        ]

encodeAnswer : (Maybe Int, Char) -> Json.Encode.Value
encodeAnswer (mNum, c) =
    Json.Encode.object
        [ ("number", encodeNullable Json.Encode.int mNum)
        , ("char", Json.Encode.string <| String.fromChar <| c)
        ]

encodePhase : Phase -> Json.Encode.Value
encodePhase phase =
    Json.Encode.string <| case phase of
                              QuoteEntry -> "QuoteEntry"
                              Anagramming -> "Anagramming"
                              CluingLettering -> "CluingLettering"

encodeNullable : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeNullable encode ma =
    case ma of
        Nothing -> Json.Encode.null
        Just a -> encode a
                                                 
decodeModel : Json.Decode.Decoder Model
decodeModel =
    Json.Decode.map3
        (\puzzle selectedClues savedPuzzles ->
             { puzzle = puzzle
             , selectedClues = selectedClues
             , savedPuzzles = savedPuzzles
             })
        (Json.Decode.field "puzzle" decodePuzzle)
        (Json.Decode.field "selectedClues" (Json.Decode.list Json.Decode.int))
        (Json.Decode.field "savedPuzzles" (Json.Decode.list decodePuzzle))
                                                 
decodePuzzle : Json.Decode.Decoder Puzzle
decodePuzzle =
    Json.Decode.map6
        (\title author quote clues phase timeModified ->
             { title = title
             , author = author
             , quote = quote
             , clues = clues
             , phase = phase
             , timeModified = timeModified
             })
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "author" Json.Decode.string)
        (Json.Decode.field "quote" Json.Decode.string)
        (Json.Decode.field "clues" (Json.Decode.list decodeClue))
        (Json.Decode.field "phase" decodePhase)
        (Json.Decode.field "timeModified" (Json.Decode.int |>
                                               Json.Decode.map Time.millisToPosix))

decodeClue : Json.Decode.Decoder Clue
decodeClue =
    Json.Decode.map2
        (\hint answer ->
             { hint = hint
             , answer = answer
             })
        (Json.Decode.field "hint" Json.Decode.string)
        (Json.Decode.field "answer" (Json.Decode.list decodeAnswer))

decodeAnswer : Json.Decode.Decoder (Maybe Int, Char)
decodeAnswer =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "number" (Json.Decode.nullable Json.Decode.int))
        (Json.Decode.field "char" (Json.Decode.string |> Json.Decode.andThen
                           (\s ->
                                case String.uncons s of
                                    Just (c, "") -> Json.Decode.succeed c
                                    _ -> Json.Decode.fail ("expected single character in answer, found '" ++ s ++ "'"))))

decodePhase : Json.Decode.Decoder Phase
decodePhase =
    Json.Decode.string |> Json.Decode.andThen
        (\s ->
             case s of
                 "QuoteEntry" -> Json.Decode.succeed QuoteEntry
                 "Anagramming" -> Json.Decode.succeed Anagramming
                 "CluingLettering" -> Json.Decode.succeed CluingLettering
                 _ -> Json.Decode.fail ("invalid phase '" ++ s ++ "'"))

-- INITIAL STATE, SUBSCRIPTIONS
                                 
init : Flags -> (Model, Cmd Msg)
init savedPuzzleJSON =
    ( savedPuzzleJSON
        |> Json.Decode.decodeValue decodeModel 
        |> Result.withDefault emptyModel {- FIXME indicate error? -}
    , Cmd.none
    )
    
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- UPDATE
                      
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Title title -> model.puzzle |> setTitle title |> fixupAnswerInitials |> asCurrentPuzzleIn model |> andSave
        Author author -> model.puzzle |> setAuthor author |> fixupAnswerInitials |> asCurrentPuzzleIn model |> andSave
        Quote quote -> model.puzzle |> setQuote quote |> fixupAnswerInitials |> asCurrentPuzzleIn model |> andSave
        Answer idx answer -> model.puzzle |> updateAnswer idx answer |> asCurrentPuzzleIn model |> andSave
        SelectClue idx -> { model | selectedClues = 
                            if 0 <= idx && idx < List.length model.puzzle.clues &&
                                not (List.member idx model.selectedClues)
                            then [idx]
                            else model.selectedClues } |> andSave
        SelectClues idxs -> { model | selectedClues =
                                  idxs |> List.filter
                                          (\idx ->
                                               0 <= idx &&
                                               idx < List.length model.puzzle.clues)
                            } |> andSave
        Hint idx hint -> model.puzzle |> updateHint idx hint |> asCurrentPuzzleIn model |> andSave
        Number idx numIdx newNum -> model.puzzle |> updateNumbering idx numIdx (newNum |> String.toInt) |> asCurrentPuzzleIn model |> andSave
        Phase phase -> model.puzzle |> setPhase phase |> asCurrentPuzzleIn model |> andSave
        Save now ->
            let newModel = model.puzzle |> setTimeModified now |> asCurrentPuzzleIn model in
            (newModel, savePuzzles (encodeModel newModel))
        NewPuzzle -> model |> popCurrentPuzzle emptyPuzzle |> andSave
        Load savedPuzzle -> model |> loadPuzzle savedPuzzle |> andSave
        ApplyNumbering nums -> model.puzzle |> applySMTNumbering nums |> asCurrentPuzzleIn model |> andSave
                            
loadPuzzle : Puzzle -> Model -> Model
loadPuzzle puzzle model =
    { model
        | savedPuzzles = model.savedPuzzles
                           |> List.filter (not << samePuzzle puzzle)
                           |> trySave model.puzzle
        , puzzle = puzzle
    }
                
popCurrentPuzzle : Puzzle -> Model -> Model
popCurrentPuzzle newPuzzle model =
    { model
        | savedPuzzles = trySave model.puzzle model.savedPuzzles
        , puzzle = newPuzzle
    }

trySave : Puzzle -> List Puzzle -> List Puzzle
trySave puzzle savedPuzzles =
    if List.all String.isEmpty [puzzle.title, puzzle.author]
    then savedPuzzles
    else insertWith comparePuzzles puzzle savedPuzzles
    
andSave : Model -> (Model, Cmd Msg)
andSave model = (model, Task.perform Save Time.now)

-- VIEW
                
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

        constraints = constraintsOfPuzzle quoteIndices puzzle
                           
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
        , section [id "saved"]
            [ div [ id "saved-puzzles" ]
                ([ div [id "saved-puzzles-header"] [ h3 [] [text "Saved puzzles"]
                          , input [ type_ "button"
                                  , onClick NewPuzzle
                                  , value "New puzzle"
                                  ]
                                []
                          ]
                 , a [] [model.puzzle |> puzzleDescription |> text]
                 ] ++
                 List.map
                     (\savedPuzzle ->
                          a [onClick (Load savedPuzzle), href "#"]
                          [savedPuzzle |> puzzleDescription |> text])
                     model.savedPuzzles)
            ]
        , section [id "quote"]
            (if quoteFixed
             then [ boardToSVG 24 quoteIndexUses model.puzzle ]
             else
                 [ textInput [tabindex 1, size 60, readonly quoteFixed]
                     "Title" puzzle.title Title
                 , textInput [tabindex 2, size 60, readonly quoteFixed]
                     "Author" puzzle.author Author
                 , textarea [ tabindex 3 {- see baseTabs below -}
                            , readonly quoteFixed
                            , placeholder "Quote"
                            , onInput Quote
                            , rows 6
                            , cols 60
                            , attribute "autocapitalize" "character"
                            , value (puzzle.quote)
                            ] 
                       []
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
                 ])
        , section [id "stats"]
            [ histToSVG quoteHist remainingHist ]
        , section [id "clues"]
            (puzzle.clues 
                |> List.map clueAnswer
                |> addInitials (String.toList initials) 
                |> addIndex 
                |> List.map (clueEntry model answersFixed))

        , section [id "clue-info"]
            (model.selectedClues |> List.map
                 (\index ->
                      let 
                        
                         clueLetter = letterFor index

                         clue = clueFor index puzzle

                         answer = clue.answer |> List.filter (Tuple.second >> Char.isAlphaNum)

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

                         clueNumbers = answer |> List.indexedMap Tuple.pair
                                              |> List.filterMap (\(ansIndex,(mNumIndex,_)) -> Maybe.map (Tuple.pair ansIndex) mNumIndex)

                         unindexedClueNumbers = clueNumbers |> List.map Tuple.second
                 
                         fullyNumbered = List.map (Tuple.second >> Just) clueNumbers == List.map Tuple.first answer

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
                         div [class "clue-detail"]
                         [ h3 [] [ clueLetter ++ ". " |> text ]
                         , textInput [ tabindex (baseTabs + List.length puzzle.clues + 1)
                                     , class "clue-hint"
                                     , value clue.hint
                                     ]
                               "Clue hint text"
                               clue.hint
                               (Hint index)

                         {- PICK UP HERE use flex layout rather than table for right wrapping -}
                         , table [class "clue-numbering"]
                             [ tr []
                                   (List.indexedMap 
                                        (\numIndex (_, c) ->
                                             let
                                                 {- FIXME hide/show certain characters? -}
                                                 fakeCharClasses =
                                                     if Char.isAlphaNum c
                                                     then [ ]
                                                     else [ class "excluded" ]
                                                 
                                                 dupClasses =
                                                     if List.member numIndex dupLetters
                                                     then [ class "double-dipped" ]
                                                     else []
                                             
                                             in
                                                 td ([class "clue-numbering-letter"] ++ fakeCharClasses ++ dupClasses)
                                                    [c |> String.fromChar |> text])
                                        answer)
                             , tr []
                                   (List.indexedMap
                                        (\numIndex (mNum, rawC) ->
                                             let
                                                 
                                                 c = Char.toUpper rawC

                                                 fakeCharClasses =
                                                     if Char.isAlphaNum c
                                                     then [ ]
                                                     else [ class "excluded" ]

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
                                             td ([class "clue-numbering-number", class validCls] ++ fakeCharClasses)
                                                (if Char.isAlphaNum c then [numberingFor numIndex mNum c] else []))
                                        answer)
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
                         ]))
        , section [id "debug"]
            [ {- constraints |> smt2OfConstraints quoteIndexWords |> text
            , Parser.run smtModelParser wcwModel |> Debug.toString |> text -}
              input [ type_ "button"
                    , onClick (Parser.run smtModelParser wcwModel |>
                               Result.withDefault [] |>
                               ApplyNumbering)
                    , value "Apply numbering"
                    ]
                    [ ]
            ]
        ]

baseTabs : Int
baseTabs = 3 {- title, author, quote -}

textInput : List (Attribute msg) -> String -> String -> (String -> msg) -> Html msg
textInput attrs p v toMsg = 
    input ([ type_ "text", placeholder p, value v, onInput toMsg ] ++ attrs) []
           
stringOfPhase : Phase -> String
stringOfPhase p =
    case p of
        QuoteEntry -> "Quote entry"
        Anagramming -> "Anagramming"
        CluingLettering -> "Cluing and lettering"

phases : List Phase
phases = [QuoteEntry, Anagramming, CluingLettering]
         
clueEntry : Model -> Bool -> (Int, (Char, String)) -> Html Msg
clueEntry model answersFixed (index, (initial, clue)) =
    let 

        initialStr = String.fromChar initial

        letter = letterFor index 
                 
        validCls = class <| if String.startsWith (String.toUpper initialStr) (String.toUpper clue)
                            then "valid"
                            else "invalid"

        selectedCls = if List.member index model.selectedClues
                       then [ class "selected" ]
                       else []
                                
        lbl = "clue-" ++ letter
    in
        div ([onClick (SelectClue index)] ++ selectedCls)
            [ label [class "clue-letter", for lbl] [text (letter ++ ". ")]
            , textInput [tabindex (index + baseTabs)
                        , name lbl
                        , validCls
                        , onFocus (SelectClue index)
                        , onClick (SelectClue index)
                        , readonly answersFixed
                        ] 
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

-- ACROSTIC FUNCTIONS

quoteIndex : Puzzle -> Int -> Maybe Char
quoteIndex puzzle index =
    puzzle.quote |> cleanChars
                 |> List.drop index
                 |> List.head

-- HISTOGRAMS
                    
type alias Hist = Dict Char Int

cleanString : String -> String
cleanString s =
    s |> String.toUpper 
      |> String.filter Char.isAlphaNum         
      {- FIXME doesn't work with diacritics, Greek, etc. -}

cleanChars : String -> List Char
cleanChars s = s |> cleanString |> String.toList
         
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

-- HISTOGRAM RENDERING
         
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

-- BOARD RENDERING

type alias Square =
    { char : Char
    , qIndex : Int
    , col : Int
    , row : Int
    }

boardToSVG : Int -> Dict Int (List (Int, Int)) -> Puzzle -> Html Msg
boardToSVG numCols quoteIndexUses puzzle =
    let
        width = 300

        quoteWords = puzzle.quote
                   |> String.words
                   |> List.filter (not << String.isEmpty)
                   |> List.map cleanString

        quoteText = String.join " " quoteWords |> String.toList

        numberedQuoteText =
            let number idx count l =
                    let row = count // numCols in
                    case l of
                        [] -> List.range (remainderBy numCols count) numCols
                              |> List.map (\col -> { char = ' '
                                                   , qIndex = -1
                                                   , col = col
                                                   , row = row
                                                   })
                        (c::rest) ->
                            let square = { char = c
                                         , qIndex = if c == ' ' then -1 else idx
                                         , col = remainderBy numCols count
                                         , row = row}
                            in
                                square :: number (idx + if c == ' ' then 0 else 1) (count + 1) rest
            in
                number 0 0 quoteText
                    
        numBoxes = List.length quoteText
                 
        boxWidth = width / (toFloat numCols)

        numRows = (numBoxes // numCols) +
                  if remainderBy numCols numBoxes == 0 then 0 else 1

        height = (toFloat numRows) * boxWidth
                    
        quoteRows =
            List.map
                (\square ->
                     let x = toFloat square.col * boxWidth
                         y = toFloat square.row * boxWidth
                         thirdBox = boxWidth / 3
                         textLength = thirdBox |> String.fromFloat

                         usedIn = Dict.get square.qIndex quoteIndexUses
                                  |> Maybe.withDefault []
                                  |> List.map Tuple.first
                                  |> Set.fromList |> Set.toList
                                  {- PICK UP HERE warning when there's more than one -}
                     in

                     Svg.g [ Svg.Attributes.class ("row-" ++ String.fromInt square.row)
                           , Svg.Attributes.class ("qIndex-" ++ String.fromInt square.qIndex)
                           , onClick (SelectClues usedIn)
                           ]
                     ([ Svg.rect
                           [ x |> String.fromFloat |> Svg.Attributes.x
                           , y |> String.fromFloat |> Svg.Attributes.y
                           , boxWidth |> String.fromFloat |> Svg.Attributes.width
                           , boxWidth |> String.fromFloat |> Svg.Attributes.height
                           , Svg.Attributes.class "board-square"
                           , Svg.Attributes.class
                               (if square.char == ' '
                                then "board-space"
                                else "board-letter")
                           , Svg.Attributes.class
                               (if List.length usedIn > 1
                                then "board-number-conflict"
                                else "")
                           ]
                           []
                      ] ++ (if square.char == ' ' then []
                            else [ Svg.text_
                                       [ x + 1 |> String.fromFloat |> Svg.Attributes.x
                                       , y + thirdBox |> String.fromFloat |> Svg.Attributes.y
                                       , Svg.Attributes.textAnchor "start"
                                       , Svg.Attributes.class "number"
                                       ]
                                       [ square.qIndex + 1 |> String.fromInt |> Svg.text ]
                                 , Svg.text_
                                       [ x + boxWidth - 1 |> String.fromFloat |> Svg.Attributes.x
                                       , y + thirdBox |> String.fromFloat |> Svg.Attributes.y
                                       , Svg.Attributes.textAnchor "end"
                                       , Svg.Attributes.class "clue-letter"
                                       ]
                                       [ usedIn 
                                         |> List.map letterFor
                                         |> String.concat
                                         |> Svg.text
                                       ]
                                 , Svg.text_
                                       [ x + (boxWidth / 2) |> String.fromFloat |> Svg.Attributes.x
                                       , y + boxWidth - 2 |> String.fromFloat |> Svg.Attributes.y
                                       , Svg.Attributes.textAnchor "middle"
                                       , Svg.Attributes.class "letter"
                                       ]
                                       [ square.char |> String.fromChar |> Svg.text ]
                                 ]))
                )
                numberedQuoteText
        
    in

        Svg.svg
            [ Svg.Attributes.viewBox 
                  ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
            , id "board" ]
            quoteRows

-- NUMBERING VIA SMT

type alias ConstraintVar = String

type alias ConstraintProblem = (List ConstraintVar, List Constraint)
    
type Constraint = IsInt ConstraintVar
                | OneOf ConstraintVar (List Int)
                | Disjoint (List ConstraintVar)
                | NotAscending (List ConstraintVar)
                | NotSameWord (List ConstraintVar)

isDefn : Constraint -> Bool
isDefn c =
    case c of
        IsInt _ -> True
        _ -> False
                  
constraintsOfPuzzle : Dict Char (List Int) -> Puzzle -> List Constraint
constraintsOfPuzzle quoteIndices puzzle =
    let
        varName clueIndex numIndex =
            "clue" ++ String.fromInt clueIndex ++ "_" ++
            "letter" ++ String.fromInt numIndex

        clueVarsByClue =
            puzzle.clues |>
            List.indexedMap
                (\clueIndex clue ->
                     clue.answer |>
                     List.filter (Tuple.second >> Char.isAlphaNum) |> {- FIXME way to consider existing numbers? -}
                     List.indexedMap Tuple.pair |>
                     List.map (\(numIndex, (_, c)) -> (varName clueIndex numIndex, c)))
                
        clueVars = List.concat clueVarsByClue
                
        charConstraints =
            clueVars |> List.concatMap
                (\(v, c) ->
                     let uses = Dict.get (Char.toUpper c) quoteIndices |>
                                Maybe.withDefault [] {- yikes -}
                     in
                         [IsInt v, OneOf v uses])

        charUses = List.foldr (\(v,c) d -> updateCons c v d) Dict.empty clueVars

        disjointnessConstraints = charUses |>
                                  Dict.values |>
                                  List.map (Disjoint)

        numberingConstraints =
            clueVarsByClue |>
            List.concatMap
                (\vs ->
                    let vars = List.map Tuple.first vs in 
                     [ NotAscending vars
                     , NotAscending (List.reverse vars)
                     , NotSameWord vars
                     ])
            
    in
    
    charConstraints ++ disjointnessConstraints ++ numberingConstraints

smtAssert : String -> String
smtAssert prop = "(assert " ++ prop ++ ")"

smtWordFun : String
smtWordFun = "word-of"

smtWordOf : String -> String
smtWordOf x = "(" ++ smtWordFun ++ " " ++ x ++")"
             
smtEq : String -> String -> String
smtEq l r = "(= " ++ l ++ " " ++ r ++ ")"
                 
smtOr : List String -> String
smtOr props =
    case props of
        [] -> "true"
        [prop] -> prop
        _ -> "(or " ++ String.join " " props ++ ")"

smtAnd : List String -> String
smtAnd props =
    case props of
        [] -> "true" -- weird, I know, but probably the right default for our case. shouldn't come up.
        [prop] -> prop
        _ -> "(and " ++ String.join " " props ++ ")"

smtNot : String -> String
smtNot prop = "(not " ++ prop ++ ")"

smtAscending : List ConstraintVar -> String
smtAscending vars =
    case vars of
        [] -> "true"
        [var] -> "true"
        [var1,var2] -> "(< " ++ var1 ++ " " ++ var2 ++ ")"
        var1::var2::rest ->
            "(and (< " ++ var1 ++ " " ++ var2 ++ ")" ++ smtAscending (var2::rest) ++ ")"

type alias SMTNumbering = List SMTNumberEntry
                
type alias SMTNumberEntry =
    { clue : Int
    , letter : Int
    , number : Int
    }

applySMTNumbering : SMTNumbering -> Puzzle -> Puzzle
applySMTNumbering nums puz =
    let apply num newPuz =
            updateNumbering num.clue num.letter (Just num.number) newPuz
    in
        List.foldr apply puz nums
        
smtModelParser : Parser SMTNumbering
smtModelParser =
    succeed identity
        |. spaces
        |. symbol "("
        |. spaces
        |= listOf smtValueParser
        |. spaces
        |. symbol ")"
        |. spaces
        |. end

smtValueParser : Parser SMTNumberEntry
smtValueParser =
    succeed (\clue letter number ->
                 { clue = clue
                 , letter = letter
                 , number = number
                 })
        |. spaces
        |. symbol "("
        |. symbol "clue"
        |= Parser.int
        |. symbol "_letter"
        |= Parser.int
        |. spaces
        |= Parser.int
        |. symbol ")"

smt2OfConstraint : Constraint -> String
smt2OfConstraint c =
    case c of
        IsInt var -> ("(declare-const " ++ var ++ " Int)")

        OneOf var ns ->
            ns |>
            List.map (\n -> smtEq var (String.fromInt n)) |>
            smtOr |>
            smtAssert

        Disjoint vars ->
            vars |>
            allPairs |>
            List.map (\(v1,v2) -> smtEq v1 v2) |>
            smtOr |>
            smtNot |>
            smtAssert

        NotAscending vars ->
            vars |>
            smtAscending |>
            smtNot |>
            smtAssert
                              
        NotSameWord vars ->
            vars |>
            allPairs |>
            List.map (\(v1,v2) -> smtEq (smtWordOf v1) (smtWordOf v2)) |>
            smtOr |>
            smtNot |>                   
            smtAssert
                      
smt2OfConstraints : Dict Int Int -> List Constraint -> String
smt2OfConstraints quoteIndexWords constraints =
    let

        (defnConstraints, assertConstraints) = List.partition isDefn constraints

        vars = defnConstraints |>
               List.filterMap
                   (\c ->
                        case c of
                            IsInt v -> Just v
                            _ -> Nothing)
                                               
        defns = defnConstraints |>
                List.map smt2OfConstraint

        wordFun =
            let
                {- METHOD 1: axiomatize (currently used, works much better) -}
                decl = "(declare-fun " ++ smtWordFun ++ " (Int) Int)"

                vals =
                    quoteIndexWords |>
                    Dict.foldr
                        (\x wordNum eqs ->
                             ("(= " ++ smtWordOf (String.fromInt x) ++ " " ++ (String.fromInt wordNum) ++ ")") :: eqs)
                        [] |>
                    smtAnd |>
                    smtAssert

                {- METHOD 2: define as a function/macro -}
                conds =
                    quoteIndexWords |>
                    Dict.foldr
                        (\x wordNum otw ->
                             "(ite (= n " ++ String.fromInt x ++ ") " ++ (String.fromInt wordNum) ++ " " ++ otw ++ ")")
                        "-1"

                        
                defn = "(define-fun " ++ smtWordFun ++ " ((n Int)) Int " ++ conds ++ ")"
                        
            in
                [decl, vals] -- [defn]
                                          
        assertions = assertConstraints |>
                     List.map smt2OfConstraint
                        
        commands = ["(set-option :produce-models true)"] ++
                   defns ++ wordFun ++ assertions ++
                   ["(check-sat)", "(get-value (" ++ String.join " " vars ++ "))"]
                           
    in
        String.join "\n" commands
        

-- UTILITY FUNCTIONS

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

listOf : Parser a -> Parser (List a)
listOf item =
    Parser.oneOf
        [ succeed (\hd tl -> hd :: tl)
            |. spaces
            |= item
            |. spaces
            |= Parser.lazy (\_ -> listOf item)
        , succeed []
        ]
                  
wcwModel : String
wcwModel = """((clue0_letter0 62)
 (clue0_letter1 78)
 (clue0_letter2 154)
 (clue0_letter3 116)
 (clue0_letter4 210)
 (clue0_letter5 67)
 (clue0_letter6 127)
 (clue0_letter7 170)
 (clue0_letter8 156)
 (clue0_letter9 137)
 (clue0_letter10 130)
 (clue1_letter0 128)
 (clue1_letter1 92)
 (clue1_letter2 99)
 (clue1_letter3 139)
 (clue1_letter4 4)
 (clue1_letter5 192)
 (clue1_letter6 59)
 (clue2_letter0 31)
 (clue2_letter1 71)
 (clue2_letter2 11)
 (clue2_letter3 33)
 (clue2_letter4 50)
 (clue3_letter0 20)
 (clue3_letter1 179)
 (clue3_letter2 42)
 (clue3_letter3 103)
 (clue3_letter4 63)
 (clue3_letter5 66)
 (clue3_letter6 105)
 (clue3_letter7 204)
 (clue3_letter8 141)
 (clue4_letter0 200)
 (clue4_letter1 72)
 (clue4_letter2 84)
 (clue4_letter3 18)
 (clue4_letter4 37)
 (clue4_letter5 98)
 (clue4_letter6 29)
 (clue4_letter7 182)
 (clue4_letter8 97)
 (clue4_letter9 196)
 (clue4_letter10 209)
 (clue4_letter11 112)
 (clue4_letter12 7)
 (clue5_letter0 189)
 (clue5_letter1 22)
 (clue5_letter2 107)
 (clue5_letter3 86)
 (clue5_letter4 57)
 (clue5_letter5 175)
 (clue5_letter6 16)
 (clue5_letter7 75)
 (clue5_letter8 5)
 (clue6_letter0 166)
 (clue6_letter1 12)
 (clue6_letter2 104)
 (clue6_letter3 40)
 (clue6_letter4 55)
 (clue6_letter5 82)
 (clue6_letter6 160)
 (clue6_letter7 186)
 (clue6_letter8 19)
 (clue6_letter9 80)
 (clue6_letter10 122)
 (clue6_letter11 220)
 (clue6_letter12 195)
 (clue6_letter13 145)
 (clue7_letter0 207)
 (clue7_letter1 149)
 (clue7_letter2 217)
 (clue7_letter3 193)
 (clue7_letter4 30)
 (clue7_letter5 191)
 (clue7_letter6 36)
 (clue8_letter0 35)
 (clue8_letter1 26)
 (clue8_letter2 70)
 (clue8_letter3 109)
 (clue9_letter0 61)
 (clue9_letter1 69)
 (clue9_letter2 25)
 (clue10_letter0 91)
 (clue10_letter1 136)
 (clue10_letter2 147)
 (clue10_letter3 43)
 (clue10_letter4 38)
 (clue10_letter5 162)
 (clue10_letter6 121)
 (clue10_letter7 118)
 (clue10_letter8 111)
 (clue10_letter9 64)
 (clue11_letter0 134)
 (clue11_letter1 27)
 (clue11_letter2 6)
 (clue11_letter3 49)
 (clue11_letter4 89)
 (clue11_letter5 142)
 (clue11_letter6 168)
 (clue11_letter7 205)
 (clue11_letter8 198)
 (clue12_letter0 214)
 (clue12_letter1 124)
 (clue12_letter2 14)
 (clue12_letter3 202)
 (clue12_letter4 88)
 (clue12_letter5 81)
 (clue12_letter6 187)
 (clue12_letter7 140)
 (clue13_letter0 203)
 (clue13_letter1 215)
 (clue13_letter2 76)
 (clue13_letter3 34)
 (clue13_letter4 23)
 (clue13_letter5 1)
 (clue13_letter6 113)
 (clue13_letter7 123)
 (clue13_letter8 165)
 (clue13_letter9 180)
 (clue14_letter0 129)
 (clue14_letter1 10)
 (clue14_letter2 21)
 (clue14_letter3 108)
 (clue14_letter4 94)
 (clue14_letter5 2)
 (clue14_letter6 167)
 (clue14_letter7 45)
 (clue15_letter0 152)
 (clue15_letter1 184)
 (clue15_letter2 125)
 (clue15_letter3 58)
 (clue15_letter4 135)
 (clue15_letter5 0)
 (clue15_letter6 132)
 (clue15_letter7 171)
 (clue15_letter8 53)
 (clue16_letter0 153)
 (clue16_letter1 216)
 (clue16_letter2 32)
 (clue16_letter3 161)
 (clue16_letter4 15)
 (clue16_letter5 83)
 (clue16_letter6 90)
 (clue16_letter7 197)
 (clue17_letter0 44)
 (clue17_letter1 173)
 (clue17_letter2 201)
 (clue17_letter3 24)
 (clue17_letter4 117)
 (clue17_letter5 158)
 (clue17_letter6 51)
 (clue17_letter7 183)
 (clue17_letter8 150)
 (clue18_letter0 133)
 (clue18_letter1 176)
 (clue18_letter2 3)
 (clue18_letter3 163)
 (clue18_letter4 106)
 (clue18_letter5 211)
 (clue18_letter6 188)
 (clue18_letter7 56)
 (clue18_letter8 54)
 (clue18_letter9 101)
 (clue18_letter10 159)
 (clue19_letter0 93)
 (clue19_letter1 155)
 (clue19_letter2 169)
 (clue19_letter3 143)
 (clue19_letter4 115)
 (clue19_letter5 178)
 (clue19_letter6 13)
 (clue19_letter7 208)
 (clue19_letter8 100)
 (clue20_letter0 41)
 (clue20_letter1 60)
 (clue20_letter2 9)
 (clue20_letter3 219)
 (clue20_letter4 114)
 (clue20_letter5 96)
 (clue20_letter6 8)
 (clue20_letter7 77)
 (clue20_letter8 65)
 (clue20_letter9 144)
 (clue21_letter0 174)
 (clue21_letter1 39)
 (clue21_letter2 218)
 (clue21_letter3 138)
 (clue22_letter0 164)
 (clue22_letter1 47)
 (clue22_letter2 120)
 (clue22_letter3 181)
 (clue22_letter4 87)
 (clue22_letter5 68)
 (clue22_letter6 212)
 (clue22_letter7 110)
 (clue22_letter8 148)
 (clue22_letter9 17)
 (clue23_letter0 102)
 (clue23_letter1 85)
 (clue23_letter2 52)
 (clue23_letter3 199)
 (clue23_letter4 79)
 (clue23_letter5 213)
 (clue23_letter6 177)
 (clue23_letter7 190)
 (clue23_letter8 157)
 (clue23_letter9 206)
 (clue23_letter10 48)
 (clue24_letter0 119)
 (clue24_letter1 194)
 (clue24_letter2 146)
 (clue24_letter3 74)
 (clue24_letter4 95)
 (clue24_letter5 126)
 (clue25_letter0 73)
 (clue25_letter1 185)
 (clue25_letter2 28)
 (clue25_letter3 151)
 (clue25_letter4 172)
 (clue25_letter5 46)
 (clue25_letter6 131))
"""
