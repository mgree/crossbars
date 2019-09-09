port module Main exposing (..)

{- TODO

   ANSWER SEARCH

     wordlists have links/hovers/tooltips with more info?
     settings display to load more wordlists
     allow user to filter by word length, letters used/avoided

     speed up somehow?
       restricting to strings of length 2 or greater sped things up a bunch

     dictionary ideas:
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

     auto insert (X wds.)
     allow markdown in clues
-}

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)
import Html.Lazy as Lazy
import Html.Keyed as Keyed

import Svg
import Svg.Attributes

import Json.Encode as Encode
import Json.Decode as Decode

import Process
import Task

import Time

import Browser

import Http

{- FIXME don't just expose everything -}
import Puzzle exposing (Puzzle, Phase(..))
import Hist exposing (Hist)
import SMT exposing (SolverState(..))
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
    
type alias Flags = Encode.Value {- saved puzzles -}

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
  | SolverResults Encode.Value
  | SolverStateChanged Encode.Value
  {- loading, etc. -}
  | TimeZone Time.Zone
  | GotWordlist WordlistSource (Result Http.Error String)
  | UpdateRecvProgress String Http.Progress
  | ClearProgress String

port savePuzzles : Encode.Value -> Cmd msg

port saveCurrentPuzzle : Encode.Value -> Cmd msg

port solveNumbering : Encode.Value -> Cmd msg

port solverResults : (Encode.Value -> msg) -> Sub msg

port solverStateChanged : (Encode.Value -> msg) -> Sub msg

-- TYPES, HELPERS                
                   
type alias Model = 
    { puzzle : Puzzle
    , pendingDelete : Bool
    , selectedClues : List Int
    , savedPuzzles : List Puzzle
    , selectedPuzzle : Maybe Puzzle
    , solverState : SolverState
    , solverResult : Maybe Solver.SMTResult
    , timeZone : Time.Zone
    , wordlist : Wordlist
    , progressBars : Dict String Float
    }

emptyModel : Model
emptyModel =
    { puzzle = Puzzle.empty
    , pendingDelete = False
    , selectedClues = []
    , savedPuzzles = []
    , selectedPuzzle = Nothing
    , solverState = SolverUnloaded
    , solverResult = Nothing
    , timeZone = Time.utc
    , wordlist = Wordlist.empty
    , progressBars = Dict.empty
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

asSolverStateIn : Model -> SolverState -> Model
asSolverStateIn model solverState = { model | solverState = solverState }    

updateProgress : String -> Float -> Model -> Model
updateProgress src progress model =
    { model | progressBars = Dict.insert src progress model.progressBars }

clearProgress : String -> Model -> Model
clearProgress src  model =
    { model | progressBars = Dict.remove src model.progressBars }

loadPuzzle : Puzzle -> Model -> Model
loadPuzzle puzzle model =
    { model
        | savedPuzzles = model.savedPuzzles
                           |> List.filter (not << Puzzle.equal puzzle)
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
    else insertWith Puzzle.compare puzzle savedPuzzles
    
andSave : SaveMode -> Model -> (Model, Cmd Msg)
andSave mode model = (model, Task.perform (Save mode) Time.now)

-- INITIAL STATE, SUBSCRIPTIONS

type alias WordlistSource =
    { url : String
    , source : String
    }

wordlists : List WordlistSource
wordlists = 
    [ { url = "words/words.txt"
      , source = "FreeBSD words list"
      }
    ]

init : Flags -> (Model, Cmd Msg)
init savedModel =
    ( savedModel
        |> Decode.decodeValue modelDecoder 
        |> Result.withDefault emptyModel {- FIXME indicate error? -}
    , Cmd.batch 
        ( Task.perform TimeZone Time.here ::
          List.map
              (\wl ->
                   Http.request
                       { method = "GET"
                       , headers = []
                       , body = Http.emptyBody
                       , url = wl.url
                       , expect = Http.expectString (GotWordlist wl)
                       , timeout = Nothing
                       , tracker = Just wl.url
                       })
              wordlists
        )
    )

modelDecoder : Decode.Decoder Model
modelDecoder =
    Decode.map2
        (\puzzle savedPuzzles ->
             { emptyModel 
             | puzzle = puzzle
             , savedPuzzles = savedPuzzles
             })
        (Decode.field "currentPuzzle" Puzzle.decoder)
        (Decode.field "savedPuzzles" (Decode.list Puzzle.decoder))
    
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ( solverStateChanged SolverStateChanged
        :: solverResults SolverResults
        :: List.map
            (\wl ->
                 Http.track wl.url (UpdateRecvProgress wl.source))
            wordlists
        )

-- UPDATE
                      
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Title title -> 
            model.puzzle |> 
            Puzzle.setTitle title |> 
            Puzzle.fixupAnswerInitials |> 
            asCurrentPuzzleIn model |> 
            andSave CurrentPuzzle
        Author author -> 
            model.puzzle |> 
            Puzzle.setAuthor author |>
            Puzzle.fixupAnswerInitials |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Quote quote -> 
            model.puzzle |>
            Puzzle.setQuote quote |>
            Puzzle.fixupAnswerInitials |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Answer idx answer -> 
            model.puzzle |>
            Puzzle.updateAnswer idx answer |>
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
            Puzzle.updateHint idx hint |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Number idx numIdx newNum -> 
            model.puzzle |>
            Puzzle.updateNumbering idx numIdx (newNum |> String.toInt) |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Phase phase ->
            model.puzzle |>
            Puzzle.setPhase phase |>
            asCurrentPuzzleIn model |>
            andSave CurrentPuzzle
        Save CurrentPuzzle now ->
            let newModel = model.puzzle |> 
                           Puzzle.setTimeModified now |> 
                           asCurrentPuzzleIn model in
            (newModel, saveCurrentPuzzle (Puzzle.encode newModel.puzzle))
        Save All _ ->
            (model, 
             Cmd.batch [ saveCurrentPuzzle (Puzzle.encode model.puzzle)
                       , savePuzzles (Encode.list Puzzle.encode model.savedPuzzles)
                       ]
            )
        NewPuzzle -> 
            model |>
            popCurrentPuzzle Puzzle.empty |>
            andSave All
        DeletePuzzle -> (model |> pendingDeletion True, Cmd.none)
        ReallyDeletePuzzle False -> (model |> pendingDeletion False, Cmd.none)
        ReallyDeletePuzzle True -> 
            Puzzle.empty |>
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
            Puzzle.clearNumbering |> 
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
            Decode.decodeValue Solver.smtResultDecoder |>
            Result.withDefault Solver.missingResult |>
            tryApplySMTNumberingTo model |>
            andSave CurrentPuzzle
        SolverStateChanged json -> 
            ( json |>
              Decode.decodeValue SMT.solverStateDecoder |>
              Result.withDefault SolverUnloaded |> {- FIXME display error -}
              asSolverStateIn model
            , Cmd.none)
        TimeZone here -> ({ model | timeZone = here }, Cmd.none)
        GotWordlist wl (Err err) -> 
            ( model
            , Cmd.none) {- FIXME display error -}
        GotWordlist wl (Ok words) -> 
            ( { model | wordlist = Wordlist.load wl.source words }
            , Cmd.none)
        UpdateRecvProgress wl (Http.Sending progress) ->
            ( model
            , Cmd.none)
        UpdateRecvProgress s (Http.Receiving progress) ->
            let recvd = Http.fractionReceived progress in
            ( model |>
              updateProgress s recvd
            , if recvd == 1.0
              then Task.perform (\() -> ClearProgress s) (Process.sleep 1000)
              else Cmd.none)
        ClearProgress s ->
            ( model |>
              clearProgress s
            , Cmd.none)

-- VIEW
                
view : Model -> Html Msg
view model = 
    let
        puzzle = model.puzzle
        
        quoteFixed = puzzle.phase /= QuoteEntry

        answersFixed = puzzle.phase /= Anagramming

        initials = Puzzle.initialism puzzle 

        initialismHist = Hist.fromString initials

        quoteHist = Hist.fromString puzzle.quote 
                                     
        missingHist = Hist.difference quoteHist initialismHist

        viable = Hist.isExhausted missingHist

        clueHist = Hist.fromString (puzzle.clues |> List.map Puzzle.clueAnswer |> String.concat)

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

        dupNumberings = Puzzle.duplicateNumberings puzzle

        completed =
            Hist.isEmpty remainingHist &&
            List.isEmpty (Puzzle.unnumbered puzzle) &&
            List.isEmpty dupNumberings &&
            List.isEmpty (Puzzle.unclued puzzle)
                    
        qIndices = Puzzle.quoteIndices puzzle

        qIndexWords = Puzzle.quoteIndexWords puzzle
                           
        qIndexUses = Puzzle.quoteIndexUses puzzle

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
                                            , value (Puzzle.stringOfPhase p)
                                            , onClick (Phase p)
                                            ]
                                            [])
                                 Puzzle.phases))
              , div [ id "next-steps" ]
                  [ text <|
                    if completed
                    then "Your puzzle is filled in! 🎉"
                    else case puzzle.phase of
                             QuoteEntry -> 
                                 "Make sure your author and title are included in your quote to move on to anagramming."
                             Anagramming ->
                                 "Finish anagramming to move on to numbering and cluing."
                             CluingLettering ->
                                 "Assign non-duplicate numbers to each letter and come up with clues."
                  ]
              ]
        , section [id "saved"]
            ([ h3 [class "header"] [text "Manage puzzles"]
             , div [ id "current-puzzle" ]
                 ([ span [] 
                       [ text "Current puzzle: "
                       , model.puzzle |> 
                         Puzzle.shortDescription |> 
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
                                         Puzzle.description model.timeZone |>
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
                             SolverUnloaded -> "Numbering solver not loaded"
                             SolverDownloading -> "Downloading numbering solver code..."
                             SolverInitializing -> "Initializing numbering solver..."
                             SolverReady -> "Numbering solver ready"
                             SolverRunning -> "Numbering solver running..."
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
                |> List.map2 Tuple.pair (String.toList initials) 
                |> addIndex 
                |> List.map 
                     (\(index, (initial, clue)) -> 
                          let 
                              answer = Puzzle.clueAnswer clue

                              initialStr = String.fromChar initial

                              letter = Puzzle.letterFor index 
                 
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
                       then div [class "explanatory"] [text "Select a clue to receive anagram suggestions."]
                       else div [] 
                             (div [class "explanatory"] [text "Type two or more characters to find matching suffixes."] ::
                              (model.selectedClues |>
                               List.map (Lazy.lazy4 anagramAssistance puzzle model.wordlist remainingHist)))
                     ]
                 CluingLettering -> 
                     (model.selectedClues |> 
                      List.map
                          (\index ->
                               let 
                                                
                                   clueLetter = Puzzle.letterFor index
                        
                                   clue = Puzzle.clueFor index puzzle
                        
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
                        
                                                            clueMention (cIdx, cNumIdx) = Puzzle.letterFor cIdx ++ ". " ++ (cNumIdx + 1 |> String.fromInt)
                        
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
                        
                                   dupWordLetters = List.map Tuple.first dupWords

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
                                                               
                                                               dupWordClasses =
                                                                   if List.member numIndex dupWordLetters
                                                                   then [ class "double-dipped" ]
                                                                   else []

                                                               dupLetterClasses =
                                                                   if List.any 
                                                                       (\(_, idxs) ->
                                                                            List.any (\idx -> idx == (index, numIndex)) idxs)
                                                                       dupNumberings
                                                                   then [ class "duplicate-numbering" ]
                                                                   else []
                                                                                    
                                                                     
                                                           in
                                                               td ([class "clue-numbering-letter"] ++ dupWordClasses ++ dupLetterClasses)
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
                                                                         if Puzzle.quoteIndex puzzle num |> 
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
            [ div [class "warnings"]
                  (let
                      tag msg ws =
                          if List.isEmpty ws
                          then ws
                          else h5 [] [text msg] :: ws

                      clueRef (cIndex, ansIndex) = 
                          span [ onClick (SelectClue cIndex) ]
                               [ text (Puzzle.letterFor cIndex ++ String.fromInt (ansIndex + 1)) ]

                      unnumbered = 
                          Puzzle.unnumbered puzzle |>
                          List.map clueRef |>
                          tag "Missing numbers"

                      dups = 
                          dupNumberings |>
                          List.map
                              (\(qIndex, idxs) ->
                                   span [ onClick (SelectClues (List.map Tuple.first idxs)) ]
                                   (  text ("Letter " ++ String.fromInt (1 + qIndex) ++ " is used in ")
                                   :: List.intersperse (text " ") (List.map clueRef idxs))) |>
                          tag "Duplicate numbers"

                      unclued = 
                          Puzzle.unclued puzzle |>
                          List.map
                              (\cIndex ->
                                   span [ onClick (SelectClue cIndex) ]
                                        [ text (Puzzle.letterFor cIndex) ]) |>
                          tag "Missing clues"

                   in
                       if puzzle.phase == CluingLettering then unnumbered ++ dups ++ unclued else [])
            , div [id "progress"]
                  (  (if not (Dict.isEmpty model.progressBars) 
                      then [h3 [] [text "Loading"]] else [])
                  ++ (model.progressBars |>
                      Dict.toList |>
                      List.map
                          (\(s,recvd) ->
                               let tag = "progress-" ++ s in
                               div []
                                 [ meter [ Html.Attributes.min "0.0"
                                         , Html.Attributes.max "1.0"
                                         , value <| String.fromFloat recvd
                                         , id tag
                                         ]
                                       []
                                 , label [for tag]
                                     [text s]
                                 ]
                          ))
                  )
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
                        [] -> List.range (remainderBy numCols count) (numCols - 1)
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
                                         |> List.map Puzzle.letterFor
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

anagramAssistance : Puzzle -> Wordlist -> Hist -> Int -> Html Msg
anagramAssistance puzzle wordlist remainingHist index =
    let

        letter = Puzzle.letterFor index

        clue = Puzzle.clueFor index puzzle

        prefix = case List.map Tuple.second clue.answer of
                     [] -> Puzzle.initialism puzzle |>
                           String.toList |>
                           List.drop index |>
                           List.take 1 |>
                           List.map Char.toUpper
                     cs -> cs

        anagrams = Wordlist.anagramsFor wordlist remainingHist prefix

        split = splitList anagrams

        anagramEntry num descr l =
            Keyed.node "div" 
                [class ("anagram-group" ++ String.fromInt num)]
                (("header"  ++ String.fromInt num, h4 [] [text descr]) ::
                 List.map (\w -> (w, div [class "anagram"] [text w])) l)

    in
        div [id ("anagram-assistance-" ++ letter)
            , class "anagrams"]
            [ Keyed.node "datalist" [id (anagramDatalistId letter)] 
                  (List.map (\anagram -> (anagram, option [value anagram] [])) anagrams)
            , anagramEntry 3 "3 letters" split.three
            , anagramEntry 4 "4 letters" split.four
            , anagramEntry 5 "5 letters" split.five
            , anagramEntry 6 "6 letters" split.six
            , anagramEntry 7 "7+ letters" split.sevenPlus
            ]
