port module Main exposing (..)

{- TODO

   ANSWER SEARCH
     /usr/share/dict/words
     Webster's 1913 -- adapt https://github.com/ponychicken/WebsterParser
     Wikipedia/Wiktionary titles
     generic JSON API?
       enter a URL
     tie in to autocomplete?

   AUTONUMBERING 
     unclear how to set timeouts in Z3 wasm---needs pthreads :(

     how good can a greedy algorithm do?
       might be faster, sometimes? (Z3 takes ~10s on my old WCW acrostic)
       can we do it ensemble style?

   NITS

     refactor:
       Crossbars.Acrostic.Creator -- main goes here
       Crossbars.Puzzle
       Crossbars.Histogram
       Crossbars.SMT

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
                   
type Phase = QuoteEntry
           | Anagramming {- quotes uneditable, show puzzle view -}
           | CluingLettering {- answers uneditable, too -}

type alias Clue =
    { hint : String
    , text : String
    , answer : List (Maybe Int, Char)
    }

clueAnswer : Clue -> String
clueAnswer c = c.answer |> List.map Tuple.second |> String.fromList

defaultClue : String -> Clue
defaultClue s = { hint = ""
                , text = s |> String.toUpper
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

shortPuzzleDescription : Puzzle -> String
shortPuzzleDescription puzzle = 
    String.toUpper puzzle.author ++ " — " ++ String.toUpper puzzle.title

puzzleDescription : Time.Zone -> Puzzle -> String
puzzleDescription here puzzle =
    shortPuzzleDescription puzzle ++ " (" ++ iso8601DateTime here puzzle.timeModified ++ ")"
    
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

clearNumbering : Puzzle -> Puzzle
clearNumbering puzzle =
    { puzzle | clues =
          puzzle.clues |>
          List.map
              (\clue ->
                   { clue | answer =
                         clue.answer |>
                         List.map (\(_, c) -> (Nothing, c))
                   })
    }

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
                       answer |> 
                       String.toUpper |> 
                       String.filter Char.isAlphaNum |>
                       String.toList |> 
                       List.map2 
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
                    
                    {clue | text = answer
                          , answer = numberedAnswer 
                    })
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

type SolverState = SolverUnloaded
                 | SolverDownloading
                 | SolverInitializing
                 | SolverReady
                 | SolverRunning

decodeSolverState : Json.Decode.Decoder SolverState
decodeSolverState =
    Json.Decode.string |>
    Json.Decode.andThen
        (\s ->
             case s of
                 "SolverUnloaded" -> Json.Decode.succeed SolverUnloaded
                 "SolverDownloading" -> Json.Decode.succeed SolverDownloading
                 "SolverInitializing" -> Json.Decode.succeed SolverInitializing
                 "SolverReady" -> Json.Decode.succeed SolverReady
                 "SolverRunning" -> Json.Decode.succeed SolverRunning
                 _ -> Json.Decode.fail ("expected solver state, found '" ++ s ++ "'"))
                   
type alias Model = 
    { puzzle : Puzzle
    , pendingDelete : Bool
    , selectedClues : List Int
    , savedPuzzles : List Puzzle
    , selectedPuzzle : Maybe Puzzle
    , solverState : SolverState
    , solverResult : Maybe SMTResult
    , timeZone : Time.Zone
    }

emptyModel : Model
emptyModel =
    { puzzle = emptyPuzzle
    , pendingDelete = False
    , selectedClues = []
    , savedPuzzles = []
    , selectedPuzzle = Nothing
    , solverState = SolverUnloaded
    , solverResult = Nothing
    , timeZone = Time.utc
    }

withSolverResult : Maybe SMTResult -> Model -> Model
withSolverResult mResult model = { model | solverResult = mResult }
    
asCurrentPuzzleIn : Model -> Puzzle -> Model
asCurrentPuzzleIn model puzzle = { model | puzzle = puzzle }    

asSelectedPuzzleIn : Model -> Maybe Puzzle -> Model
asSelectedPuzzleIn model puzzle = { model | selectedPuzzle = puzzle }

clearSelectedPuzzle : Model -> Model
clearSelectedPuzzle model = { model | selectedPuzzle = Nothing }

pendingDeletion : Bool -> Model -> Model
pendingDeletion pending model = { model | pendingDelete = pending }

asSolverStateIn : Model -> SolverState -> Model
asSolverStateIn model solverState = { model | solverState = solverState }    

-- PUZZLE SAVING

encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ("encodePuzzle", encodePuzzle model.puzzle)
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
        , ("text", Json.Encode.string clue.text)
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
    Json.Decode.map2
        (\puzzle savedPuzzles ->
             { puzzle = puzzle
             , pendingDelete = False
             , selectedClues = []
             , savedPuzzles = savedPuzzles
             , selectedPuzzle = Nothing
             , solverState = SolverUnloaded
             , solverResult = Nothing
             , timeZone = Time.utc
             })
        (Json.Decode.field "currentPuzzle" decodePuzzle)
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
    Json.Decode.map3
        (\hint text answer ->
             { hint = hint
             , text = text
             , answer = answer
             })
        (Json.Decode.field "hint" Json.Decode.string)
        (Json.Decode.field "text" Json.Decode.string)
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
init savedModel =
    ( savedModel
        |> Json.Decode.decodeValue decodeModel 
        |> Result.withDefault emptyModel {- FIXME indicate error? -}
    , Task.perform TimeZone Time.here
    )
    
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
             constraintsOfPuzzle (quoteIndices model.puzzle) |> 
             smt2OfConstraints (quoteIndexWords model.puzzle) |>
             Json.Encode.string |> 
             solveNumbering)
        SolverResults json ->
            json |>
            Json.Decode.decodeValue decodeSMTResult |>
            Result.withDefault smtMissingResult |>
            tryApplySMTNumberingTo model |>
            andSave CurrentPuzzle
        SolverStateChanged json -> 
            ( json |>
              Json.Decode.decodeValue decodeSolverState |>
              Result.withDefault SolverUnloaded |>
              asSolverStateIn model
            , Cmd.none)
                                    {- FIXME display error -}
        TimeZone here -> ({ model | timeZone = here }, Cmd.none)

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

-- VIEW
                
view : Model -> Html Msg
view model = 
    let
        puzzle = model.puzzle
        
        quoteFixed = puzzle.phase /= QuoteEntry

        answersFixed = puzzle.phase /= Anagramming

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
                                         SMTFailed -> "Could not find a numbering (" ++ time ++ "). 😦"
                                         SMTTimeout -> "Timed out (" ++ time ++ "). ⏲"
                                         SMTOk _ -> "Success! 🎊 The puzzle has been automatically numbered in " ++ time ++ "."
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
                  , histToSVG quoteHist remainingHist ])
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
                                    List.map (anagramAssistance model remainingHist))
                     {- PICK UP HERE 

                        display results from each wordlist, sorted by size
                          wordlists have links/hovers/tooltips with more info?

                        Model has some Wordlist entries in it
                        
                        settings display here to load more wordlists
                            
                        allow user to filter by word length, letters used/avoided
                      -}
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
           
stringOfPhase : Phase -> String
stringOfPhase p =
    case p of
        QuoteEntry -> "Quote entry"
        Anagramming -> "Anagramming"
        CluingLettering -> "Cluing and lettering"

phases : List Phase
phases = [QuoteEntry, Anagramming, CluingLettering]
         
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

addInitials : List Char -> List a -> List (Char, a)
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

quoteIndices : Puzzle -> Dict Char (List Int)
quoteIndices puzzle =
    puzzle.quote |> cleanChars
                 |> List.indexedMap (\i c -> (c, i))
                 |> List.foldr (\(c, i) d -> updateCons c i d) Dict.empty

quoteIndexWords : Puzzle -> Dict Int Int
quoteIndexWords puzzle =
    puzzle.quote |> String.words
                 |> List.map cleanChars
                 |> List.filter (not << List.isEmpty)
                 |> List.indexedMap (\i w -> List.repeat (List.length w) i)
                 |> List.concat
                 |> List.indexedMap Tuple.pair
                 |> Dict.fromList

quoteIndexUses : Puzzle -> Dict Int (List (Int, Int))
quoteIndexUses puzzle = 
    puzzle.clues |> List.indexedMap (\i clue -> 
                                        List.foldr 
                                          (\(numIndex, (mNum,_)) d ->
                                               case mNum of
                                                   Nothing -> d
                                                   Just num -> updateCons num (i, numIndex) d)
                                          Dict.empty
                                          (clue.answer |> List.indexedMap Tuple.pair))
                |> mergeConsMany


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

foundInHist : Hist -> String -> Bool
foundInHist hist s = 
    s |>
    letterHist |>
    histDifference hist |>
    isExhaustedHist

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

-- NUMBERING VIA SMT

type alias ConstraintVar = String

type alias ConstraintProblem = (List ConstraintVar, List Constraint)
    
type Constraint = IsInt ConstraintVar
                | OneOf ConstraintVar (List Int)
                | Distinct (List ConstraintVar)
                | NotAscending (List ConstraintVar)
                | NotSameWord (List ConstraintVar)

isDefn : Constraint -> Bool
isDefn c =
    case c of
        IsInt _ -> True
        _ -> False
                  
constraintsOfPuzzle : Dict Char (List Int) -> Puzzle -> List Constraint
constraintsOfPuzzle qIndices puzzle =
    let
        varName clueIndex numIndex =
            "clue" ++ String.fromInt clueIndex ++ "_" ++
            "letter" ++ String.fromInt numIndex

        clueVarsByClue =
            puzzle.clues |>
            List.indexedMap
                (\clueIndex clue ->
                     clue.answer |> {- FIXME way to consider existing numbers? -}
                     List.indexedMap Tuple.pair |>
                     List.map (\(numIndex, (_, c)) -> (varName clueIndex numIndex, c)))
                
        clueVars = List.concat clueVarsByClue
                
        charConstraints =
            clueVars |> List.concatMap
                (\(v, c) ->
                     let uses = Dict.get (Char.toUpper c) qIndices |>
                                Maybe.withDefault [] {- yikes -}
                     in
                         [IsInt v, OneOf v uses])

        charUses = List.foldr (\(v,c) d -> updateCons c v d) Dict.empty clueVars

        disjointnessConstraints = charUses |>
                                  Dict.values |>
                                  List.map Distinct

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
        _ -> "(< " ++ String.join " " vars ++ ")"

smtDistinct : List ConstraintVar -> String
smtDistinct vars =
    case vars of
        [] -> "true"
        [var] -> "true"
        _ -> "(distinct " ++ String.join " " vars ++ ")"

type alias SMTResult =
    { answer : SMTAnswer
    , elapsed : Int {- millis -}
    }

type SMTAnswer = SMTOk SMTNumbering
               | SMTTimeout
               | SMTFailed

smtMissingResult : SMTResult
smtMissingResult = { answer = SMTFailed
                   , elapsed = 0
                   }

type alias SMTNumbering = List SMTNumberEntry
                
type alias SMTNumberEntry =
    { clue : Int
    , letter : Int
    , number : Int
    }

tryApplySMTNumberingTo : Model -> SMTResult -> Model
tryApplySMTNumberingTo model result =
    (case result.answer of
         SMTFailed -> model
         SMTTimeout -> model
         SMTOk nums -> model.puzzle |>
                       applySMTNumbering nums |>
                       asCurrentPuzzleIn model) |>
    withSolverResult (Just result)

applySMTNumbering : SMTNumbering -> Puzzle -> Puzzle
applySMTNumbering nums puz =
    let apply num newPuz =
            updateNumbering num.clue num.letter (Just num.number) newPuz
    in
        List.foldr apply puz nums

decodeSMTResult : Json.Decode.Decoder SMTResult
decodeSMTResult = 
    Json.Decode.map2 
        (\elapsed stdout ->
             let answer = String.join "\n" stdout |>
                          Parser.run smtAnswerParser |>
                          Result.withDefault SMTFailed
             in { answer = answer
                , elapsed = elapsed
                })
        (Json.Decode.field "elapsed" Json.Decode.int)
        (Json.Decode.field "stdout" (Json.Decode.list Json.Decode.string))

smtAnswerParser : Parser SMTAnswer
smtAnswerParser =
  Parser.oneOf
      [ succeed SMTFailed
        |. symbol "unsat"
      , succeed SMTTimeout
        |. symbol "unknown"
      , succeed SMTOk
        |. symbol "sat"
        |. spaces
        |= smtModelParser
      ]
        
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

        Distinct [] -> smtAssert "true"
        Distinct vars -> 
            vars |>
            smtDistinct |>
            smtAssert

        NotAscending vars ->
            vars |>
            smtAscending |>
            smtNot |>
            smtAssert
                 
        NotSameWord [] -> smtAssert "true"
        NotSameWord [_] -> smtAssert "true"             
        NotSameWord vars ->
            vars |>
            List.map smtWordOf |>
            smtDistinct |>
            smtAssert

smt2OfConstraints : Dict Int Int -> List Constraint -> String
smt2OfConstraints qIndexWords constraints =
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
                    qIndexWords |>
                    Dict.foldr
                        (\x wordNum eqs ->
                             ("(= " ++ smtWordOf (String.fromInt x) ++ " " ++ (String.fromInt wordNum) ++ ")") :: eqs)
                        [] |>
                    smtAnd |>
                    smtAssert

                {- METHOD 2: define as a function/macro -}
                conds =
                    qIndexWords |>
                    Dict.foldr
                        (\x wordNum otw ->
                             "(ite (= n " ++ String.fromInt x ++ ") " ++ (String.fromInt wordNum) ++ " " ++ otw ++ ")")
                        "-1"
                        
                defn = "(define-fun " ++ smtWordFun ++ " ((n Int)) Int " ++ conds ++ ")"
                        
            in
                [decl, vals] -- [defn]
                                          
        assertions = assertConstraints |>
                     List.map smt2OfConstraint
                        
        commands = [ {- "(set-option :timeout 2000)"
                   , -} "(set-option :produce-models true)"] ++
                   defns ++ wordFun ++ assertions ++
                   ["(check-sat)", "(get-value (" ++ String.join " " vars ++ "))"]
                           
    in
        String.join "\n" commands

-- WORDLIST FUNCTIONS

anagramDatalistId : String -> String
anagramDatalistId letter = "clue-anagrams-" ++ letter

anagramAssistance : Model -> Hist -> Int -> Html Msg
anagramAssistance model remainingHist index =
    let

        letter = letterFor index

        clue = clueFor index model.puzzle

        prefix = case List.map Tuple.second clue.answer of
                     [] -> initialism model.puzzle |>
                           String.toList |>
                           List.drop index |>
                           List.take 1 |>
                           List.map Char.toUpper
                     cs -> cs

        anagrams = anagramsFor testingWordlist remainingHist prefix

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

{- FIXME need a prefix tree or something -}
type alias Wordlist = Dict Char (List String)

anagramsFor : Wordlist -> Hist -> List Char -> List String
anagramsFor wl remainingHist prefix =
    case prefix of 
        [] -> []
        c::rest ->
            let s = String.fromList prefix in
            Dict.get c wl |>
            Maybe.withDefault [] |>
            List.filter (String.startsWith s) |>
            List.filter (String.dropLeft (String.length s) >> 
                         foundInHist remainingHist)

testingWordlist : Wordlist
testingWordlist = Dict.fromList <|
    [ ('A', ["ABC", "ABCD", "ABF", "ABD", "ACF", "AFC"])
    , ('B', ["BCD", "BFD", "BDF", "BFF"])
    , ('C', [])
    , ('D', [])
    , ('E', [])
    , ('F', [])
    , ('G', [])
    , ('H', [])
    , ('I', [])
    , ('J', [])
    , ('K', [])
    , ('L', [])
    , ('M', [])
    , ('N', [])
    , ('O', [])
    , ('P', [])
    , ('Q', [])
    , ('R', [])
    , ('S', [])
    , ('T', [])
    , ('U', [])
    , ('V', [])
    , ('W', [])
    , ('X', [])
    , ('Y', [])
    , ('Z', [])
    ]

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
                     
