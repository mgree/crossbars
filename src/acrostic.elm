port module Main exposing (..)

{- TODO

   ANSWER SEARCH

     wordlists have links/hovers/tooltips with more info?
     Model has some Wordlist entries in it
     settings display here to load more wordlists
     allow user to filter by word length, letters used/avoided

     dictionary ideas:
       /usr/share/dict/words
       Webster's 1913 -- adapt https://github.com/ponychicken/WebsterParser
       Wikipedia/Wiktionary titles
     dictionary format validator

   AUTONUMBERING 
     unclear how to set timeouts in Z3 wasm---needs pthreads :(
       build w/o pthreads doesn't work

     how good can a greedy algorithm do?
       might be faster, sometimes? (Z3 takes ~10s on my old WCW acrostic)
       can we do it ensemble style?

   NITS

     clicking on a square highlights selected letters in the clue?
     way to control escaped characters in the quote
     right-aligned wrapping of long clues
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

import Task

import Time

import Browser

{- FIXME don't just expose everything -}
import Puzzle exposing (..)
import Hist exposing (Hist)
import SMT
import Solver
import Util exposing (..)
import Wordlist exposing (Wordlist)

-- MAIN                    

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

-- MESSAGES, PORTS, FLAGS
    
type alias Flags = Json.Encode.Value {- saved puzzles -}

type SaveMode = CurrentPuzzle
              | All

type Msg =
  {- editing -}
    Title String
  | Author String
  | Quote String
  | Answer Int String
  | SelectClue Int
  | SelectClues (List Int)
  | Hint Int String
  | Number Int Int String
  | Phase Phase
  {- puzzle management -}
  | Save SaveMode Time.Posix
  | NewPuzzle
  | DeletePuzzle
  | ReallyDeletePuzzle Bool
  | SelectPuzzle String
  | LoadPuzzle Puzzle
  {- numbering via Z3.wasm -}
  | ClearNumbering
  | SolveNumbering
  | SolverResults Json.Encode.Value
  | SolverStateChanged Json.Encode.Value
  {- loading, etc. -}
  | TimeZone Time.Zone

port savePuzzles : Json.Encode.Value -> Cmd msg

port saveCurrentPuzzle : Json.Encode.Value -> Cmd msg

port solveNumbering : Json.Encode.Value -> Cmd msg

port solverResults : (Json.Encode.Value -> msg) -> Sub msg

port solverStateChanged : (Json.Encode.Value -> msg) -> Sub msg

-- TYPES, HELPERS                
                   
type alias Model = 
    { puzzle : Puzzle
    , pendingDelete : Bool
    , selectedClues : List Int
    , savedPuzzles : List Puzzle
    , selectedPuzzle : Maybe Puzzle
    , solverState : SMT.SolverState
    , solverResult : Maybe Solver.SMTResult
    , timeZone : Time.Zone
    }

emptyModel : Model
emptyModel =
    { puzzle = emptyPuzzle
    , pendingDelete = False
    , selectedClues = []
    , savedPuzzles = []
    , selectedPuzzle = Nothing
    , solverState = SMT.SolverUnloaded
    , solverResult = Nothing
    , timeZone = Time.utc
    }
  
asCurrentPuzzleIn : Model -> Puzzle -> Model
asCurrentPuzzleIn model puzzle = { model | puzzle = puzzle }    

asSelectedPuzzleIn : Model -> Maybe Puzzle -> Model
asSelectedPuzzleIn model puzzle = { model | selectedPuzzle = puzzle }

clearSelectedPuzzle : Model -> Model
clearSelectedPuzzle model = { model | selectedPuzzle = Nothing }

pendingDeletion : Bool -> Model -> Model
pendingDeletion pending model = { model | pendingDelete = pending }

withSolverResult : Maybe Solver.SMTResult -> Model -> Model
withSolverResult mResult model = { model | solverResult = mResult }

tryApplySMTNumberingTo : Model -> Solver.SMTResult -> Model
tryApplySMTNumberingTo model result =
    (case result.answer of
         Solver.SMTFailed -> model
         Solver.SMTTimeout -> model
         Solver.SMTOk nums -> model.puzzle |>
                              Solver.applySMTNumbering nums |>
                              asCurrentPuzzleIn model) |>
    withSolverResult (Just result)

asSolverStateIn : Model -> SMT.SolverState -> Model
asSolverStateIn model solverState = { model | solverState = solverState }    

loadPuzzle : Puzzle -> Model -> Model
loadPuzzle puzzle model =
    { model
        | savedPuzzles = model.savedPuzzles
                           |> List.filter (not << samePuzzle puzzle)
                           |> trySave model.puzzle
        , puzzle = puzzle
        , selectedClues = []
    } |> withSolverResult Nothing
                
popCurrentPuzzle : Puzzle -> Model -> Model
popCurrentPuzzle newPuzzle model =
    { model
        | savedPuzzles = trySave model.puzzle model.savedPuzzles
        , puzzle = newPuzzle
    } |> withSolverResult Nothing

trySave : Puzzle -> List Puzzle -> List Puzzle
trySave puzzle savedPuzzles =
    if List.all String.isEmpty [puzzle.title, puzzle.author]
    then savedPuzzles
    else insertWith comparePuzzles puzzle savedPuzzles
    
andSave : SaveMode -> Model -> (Model, Cmd Msg)
andSave mode model = (model, Task.perform (Save mode) Time.now)

-- INITIAL STATE, SUBSCRIPTIONS
                                 
init : Flags -> (Model, Cmd Msg)
init savedModel =
    ( savedModel
        |> Json.Decode.decodeValue decodeModel 
        |> Result.withDefault emptyModel {- FIXME indicate error? -}
    , Task.perform TimeZone Time.here
    )

decodeModel : Json.Decode.Decoder Model
decodeModel =
    Json.Decode.map2
        (\puzzle savedPuzzles ->
             { puzzle = puzzle
             , pendingDelete = False
             , selectedClues = []
             , savedPuzzles = savedPuzzles
             , selectedPuzzle = Nothing
             , solverState = SMT.SolverUnloaded
             , solverResult = Nothing
             , timeZone = Time.utc
             })
        (Json.Decode.field "currentPuzzle" decodePuzzle)
        (Json.Decode.field "savedPuzzles" (Json.Decode.list decodePuzzle))
    
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ solverStateChanged SolverStateChanged
        , solverResults SolverResults
        ]

-- UPDATE
                      
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Title title -> 
            model.puzzle |> 
            setTitle title |> 
            fixupAnswerInitials |> 
            asCurrentPuzzleIn model |> 
            andSave CurrentPuzzle
        Author author -> 
            model.puzzle |> 
            setAuthor author |>
            fixupAnswerInitials |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Quote quote -> 
            model.puzzle |>
            setQuote quote |>
            fixupAnswerInitials |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Answer idx answer -> 
            model.puzzle |>
            updateAnswer idx answer |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        SelectClue idx -> 
            { model | selectedClues = 
                  if 0 <= idx && idx < List.length model.puzzle.clues &&
                      not (List.member idx model.selectedClues)
                  then [idx]
                  else model.selectedClues } |> 
            andSave CurrentPuzzle
        SelectClues idxs -> 
            { model | selectedClues =
                  idxs |> 
                  List.filter
                      (\idx -> 0 <= idx && idx < List.length model.puzzle.clues)
            } |> 
            andSave CurrentPuzzle
        Hint idx hint -> 
            model.puzzle |>
            updateHint idx hint |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Number idx numIdx newNum -> 
            model.puzzle |>
            updateNumbering idx numIdx (newNum |> String.toInt) |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Phase phase ->
            model.puzzle |>
            setPhase phase |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Save CurrentPuzzle now ->
            let newModel = model.puzzle |> setTimeModified now |> asCurrentPuzzleIn model in
            (newModel, saveCurrentPuzzle (encodePuzzle newModel.puzzle))
        Save All _ ->
            (model, 
             Cmd.batch [ saveCurrentPuzzle (encodePuzzle model.puzzle)
                       , savePuzzles (Json.Encode.list encodePuzzle model.savedPuzzles)
                       ]
            )
        NewPuzzle -> 
            model |>
            popCurrentPuzzle emptyPuzzle |>
            andSave All
        DeletePuzzle -> (model |> pendingDeletion True, Cmd.none)
        ReallyDeletePuzzle False -> (model |> pendingDeletion False, Cmd.none)
        ReallyDeletePuzzle True -> 
            emptyPuzzle |>
            asCurrentPuzzleIn model |>
            pendingDeletion False |>
            andSave CurrentPuzzle
        SelectPuzzle sIndex ->
            ( sIndex |>
              String.toInt |>
              Maybe.withDefault (List.length model.savedPuzzles) |>
              (\index -> List.drop index model.savedPuzzles) |>
              List.head |>
              asSelectedPuzzleIn model
            , Cmd.none
            )
        LoadPuzzle savedPuzzle -> 
            model |>
            clearSelectedPuzzle |>
            loadPuzzle savedPuzzle |> 
            andSave All
        ClearNumbering -> 
            model.puzzle |> 
            clearNumbering |> 
            asCurrentPuzzleIn model |> 
            withSolverResult Nothing |>
            andSave CurrentPuzzle
        SolveNumbering ->
            (model, 
             model.puzzle |> 
             Solver.generateNumberingProblem |>
             solveNumbering)
        SolverResults json ->
            json |>
            Json.Decode.decodeValue Solver.decodeSMTResult |>
            Result.withDefault Solver.missingResult |>
            tryApplySMTNumberingTo model |>
            andSave CurrentPuzzle
        SolverStateChanged json -> 
            ( json |>
              Json.Decode.decodeValue SMT.decodeSolverState |>
              Result.withDefault SMT.SolverUnloaded |>
              asSolverStateIn model
            , Cmd.none)
                                    {- FIXME display error -}
        TimeZone here -> ({ model | timeZone = here }, Cmd.none)

-- VIEW
                
view : Model -> Html Msg
view model = 
    let
        puzzle = model.puzzle
        
        quoteFixed = puzzle.phase /= QuoteEntry

        answersFixed = puzzle.phase /= Anagramming

        initials = initialism puzzle 

        initialismHist = Hist.fromString initials

        quoteHist = Hist.fromString puzzle.quote 
                                     
        missingHist = Hist.difference quoteHist initialismHist

        viable = Hist.isExhausted missingHist

        clueHist = Hist.fromString (puzzle.clues |> List.map clueAnswer |> String.concat)

        remainingHist = Hist.difference clueHist quoteHist

        readyForPhase phase =
            puzzle.phase == phase ||
                case phase of
                    QuoteEntry -> True
                    Anagramming -> viable &&
                                   not (Hist.isEmpty quoteHist)
                    CluingLettering -> viable &&
                                       not (Hist.isEmpty quoteHist) &&
                                       Hist.isEmpty remainingHist
                    
        qIndices = quoteIndices puzzle

        qIndexWords = quoteIndexWords puzzle
                           
        qIndexUses = quoteIndexUses puzzle

    in

    div [id "crossbars-wrapper"]
        [ section [id "overview"]
              [ h3 [class "header"] [text "Crossbars — Acrostic Constructor"]
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
            ([ h3 [class "header"] [text "Manage puzzles"]
             , div [ id "current-puzzle" ]
                 ([ span [] 
                       [ text "Current puzzle: "
                       , model.puzzle |> 
                         shortPuzzleDescription |> 
                         text
                       ]
                  ] ++ (if model.pendingDelete
                        then [ ]
                        else [ input [ type_ "button"
                                     , onClick DeletePuzzle
                                     , value "Delete current puzzle"
                                     ]
                                   []
                             ]))
             ] ++
             (if model.pendingDelete 
              then [ div [ id "deletion-prompt" ]
                         [ span []
                               [text "Are you sure? Deleted puzzles are gone forever."]
                         , input [ type_ "button"
                                 , onClick (ReallyDeletePuzzle False)
                                 , value "Cancel"
                                 ]
                               []
                         ,  input [ type_ "button"
                                  , onClick (ReallyDeletePuzzle True)
                                  , value "Delete current puzzle"
                                  ]
                               []
                         ]
                   ]
              else [])
               ++
             [ div [ id "saved-puzzles" ]
                 ([ input [ id "new-puzzle"
                          , type_ "button"
                          , onClick NewPuzzle
                          , value "New puzzle"
                          ]
                        []
                  ] ++
                  [ select [ id "saved-puzzle-list"
                           , onInput SelectPuzzle
                           ]
                        (option [ value ""
                                , selected (model.selectedPuzzle == Nothing)
                                ] 
                                [text "Select saved puzzle..."] ::
                         (List.indexedMap
                              (\index savedPuzzle ->
                                   (option 
                                        [ index |> String.fromInt |> value
                                        , selected (model.selectedPuzzle == Just savedPuzzle)
                                        ]
                                        [savedPuzzle |> 
                                         puzzleDescription model.timeZone |>
                                         text]))
                              model.savedPuzzles))
                  , input ([ type_ "button"
                           , value "Load puzzle"
                           ] ++
                           (case model.selectedPuzzle of
                                Nothing -> []
                                Just sPuz -> [ onClick (LoadPuzzle sPuz) ]))
                        []
                  ])
             ])
        , section [id "quote"]
            (if quoteFixed
             then [ boardToSVG 24 qIndexUses model.puzzle ]
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
                               else text ("The quote does not have some letters the initialism needs: " ++ Hist.toShortString missingHist)
                             ]
                       , span [class "count"] 
                           [ text "Total letters: "
                           , quoteHist |> Hist.count |> String.fromInt |> text]
                       , span [class "count"] 
                           [ text "Remaining letters: "
                           , remainingHist |> Hist.count |> String.fromInt |> text]
                       ]
                 ])
        , section [id "detail"]
            (if puzzle.phase == CluingLettering
             then [ h3 [class "header"] [text "Numbering solver"]
                  , div [id "solver-state"]
                        [text <|
                         case model.solverState of
                             SMT.SolverUnloaded -> "Numbering solver not loaded"
                             SMT.SolverDownloading -> "Downloading numbering solver code..."
                             SMT.SolverInitializing -> "Initializing numbering solver..."
                             SMT.SolverReady -> "Numbering solver ready"
                             SMT.SolverRunning -> "Numbering solver running..."
                        ]
                  , div [id "solver-result"]
                        [text <|
                             case model.solverResult of
                                 Nothing -> ""
                                 Just result ->
                                     let time = 
                                             if result.elapsed >= 1000
                                             then let ss = String.fromInt (result.elapsed // 1000)
                                                      ms = String.padLeft 3 '0' (String.fromInt (modBy 1000 result.elapsed))
                                                  in ss ++ "." ++ ms ++ "s"
                                             else String.fromInt result.elapsed ++ "ms" 
                                     in
                                     case result.answer of
                                         Solver.SMTFailed -> "Could not find a numbering (" ++ time ++ "). 😦"
                                         Solver.SMTTimeout -> "Timed out (" ++ time ++ "). ⏲"
                                         Solver.SMTOk _ -> "Success! 🎊 The puzzle has been automatically numbered in " ++ time ++ "."
                        ]
                  , input [ type_ "button"
                          , onClick ClearNumbering
                          , value "Clear numbering" ]
                          []
                  , input [ type_ "button"
                          , onClick SolveNumbering
                          , value "Automatically assign numbers"
                          ]
                          []
                  ]
             else [ h3 [class "header"] [text "Letters remaining"]
                  , Hist.toSVG quoteHist remainingHist ])
        , section [id "clues"]
            (puzzle.clues 
                |> addInitials (String.toList initials) 
                |> addIndex 
                |> List.map 
                     (\(index, (initial, clue)) -> 
                          let 
                              answer = clueAnswer clue

                              initialStr = String.fromChar initial

                              letter = letterFor index 
                 
                              validCls = class <| 
                                         if String.startsWith (String.toUpper initialStr)  (String.toUpper answer)
                                         then "valid"
                                         else "invalid"

                              selectedCls = if quoteFixed && List.member index model.selectedClues
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
                                              , list (anagramDatalistId letter)
                                              ] 
                                        (initialStr ++ "...") clue.text (Answer index)
                                  ]))

        , section [id "clue-info"]
            (case puzzle.phase of
                 QuoteEntry -> []
                 Anagramming -> 
                     [ h3 [class "header"] [text "Anagrams"]
                     , if List.isEmpty model.selectedClues
                       then span [] [text "Select a clue to receive anagram suggestions."]
                       else div [] (model.selectedClues |>
                                    List.map (anagramAssistance puzzle remainingHist))
                     ]
                 CluingLettering -> 
                     (model.selectedClues |> 
                      List.map
                          (\index ->
                               let 
                                                
                                   clueLetter = letterFor index
                        
                                   clue = clueFor index puzzle
                        
                                   answer = clue.answer
                        
                                   numberingFor numIndex mNum c = 
                                       select [id ("clue-numbering-" ++ String.fromInt index ++ "-" ++ String.fromInt numIndex)
                                              , onInput (Number index numIndex)]
                                         ([option [ value ""
                                                  , selected (mNum == Nothing)] 
                                               [text "###"]] ++
                                              (List.map
                                                   (\qIndex ->
                                                        let 
                                                            
                                                            uses = Dict.get qIndex qIndexUses |> 
                                                                   Maybe.withDefault [] |> 
                                                                   List.filter (\(uIdx, uNumIdx) -> uIdx /= index || (uIdx == index && uNumIdx /= numIndex))
                        
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
                                                   (Dict.get c qIndices |> Maybe.withDefault [])))
                        
                                   clueNumbers = answer |> 
                                                 List.indexedMap Tuple.pair |> 
                                                 List.filterMap (\(ansIndex,(mNumIndex,_)) -> Maybe.map (Tuple.pair ansIndex) mNumIndex)
                        
                                   unindexedClueNumbers = clueNumbers |> List.map Tuple.second
                                         
                                   fullyNumbered = List.map (Tuple.second >> Just) clueNumbers == List.map Tuple.first answer
                        
                                   clueWords = List.filterMap
                                                 (\(ansIndex, numIndex) ->
                                                      Dict.get numIndex qIndexWords |> Maybe.map (Tuple.pair ansIndex))
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
                                                      answer)
                                           , tr []
                                               (List.indexedMap
                                                    (\numIndex (mNum, rawC) ->
                                                         let
                                                             
                                                             c = Char.toUpper rawC
                        
                                                             validCls = 
                                                                 case mNum of
                                                                     Nothing -> "unentered"
                                                                     Just num ->
                                                                         if quoteIndex puzzle num |> 
                                                                            Maybe.map (\qC -> c == Char.toUpper qC) |> 
                                                                            Maybe.withDefault False
                                                                         then "valid"
                                                                         else "invalid"
                                                                                                            
                                                         in
                                                             td [class "clue-numbering-number", class validCls]
                                                         [numberingFor numIndex mNum c])
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
                                       ])))
        , section [id "messages"]
            [ 
            ]
        , section [id "debug", style "display" "none"]
            [ 
            ]
        ]

baseTabs : Int
baseTabs = 3 {- title, author, quote -}

textInput : List (Attribute msg) -> String -> String -> (String -> msg) -> Html msg
textInput attrs p v toMsg = 
    input ([ type_ "text", placeholder p, value v, onInput toMsg ] ++ attrs) []

-- BOARD RENDERING

type alias Square =
    { char : Char
    , qIndex : Int
    , col : Int
    , row : Int
    }

boardToSVG : Int -> Dict Int (List (Int, Int)) -> Puzzle -> Html Msg
boardToSVG numCols qIndexUses puzzle =
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

                         usedIn = Dict.get square.qIndex qIndexUses
                                  |> Maybe.withDefault []
                                  |> List.map Tuple.first
                                  |> Set.fromList |> Set.toList
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

-- ANAGRAMS

anagramDatalistId : String -> String
anagramDatalistId letter = "clue-anagrams-" ++ letter

anagramAssistance : Puzzle -> Hist -> Int -> Html Msg
anagramAssistance puzzle remainingHist index =
    let

        letter = letterFor index

        clue = clueFor index puzzle

        prefix = case List.map Tuple.second clue.answer of
                     [] -> initialism puzzle |>
                           String.toList |>
                           List.drop index |>
                           List.take 1 |>
                           List.map Char.toUpper
                     cs -> cs

        anagrams = Wordlist.anagramsFor Wordlist.empty remainingHist prefix

        split = splitList anagrams

        anagramEntry num descr l =
            div [class ("anagram-group" ++ String.fromInt num)]
                ([h4 [] [text descr]] ++
                 List.map (text >> List.singleton >> div [class "anagram"]) l)
    in
        div [id ("anagram-assistance-" ++ letter)
            , class "anagrams"]
            [ datalist [id (anagramDatalistId letter)] 
                  (List.map (\anagram -> option [value anagram] []) anagrams)
            , anagramEntry 3 "3 letters" split.three
            , anagramEntry 4 "4 letters" split.four
            , anagramEntry 5 "5 letters" split.five
            , anagramEntry 6 "6 letters" split.six
            , anagramEntry 7 "7+ letters" split.sevenPlus
            ]
