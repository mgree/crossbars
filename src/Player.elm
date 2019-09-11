port module Main exposing (..)

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)

import Svg
import Svg.Attributes

import Browser
import Browser.Events
import Browser.Dom

import Json.Decode as Decode
import Json.Encode as Encode

import Task

import File exposing (File)
import File.Select as Select
import Url

import Puzzle
import Util exposing (..)

{- PICK UP HERE

   dropdown for loading from playedPuzzles
     partition based on completed vs. not

   support for markdown in hints

   loading games
     uuencoded (from URL, copy/paste?)

   more concise blank format (JSON is v wasteful)

 -}

-- TYPES

type alias Flags = Encode.Value

type Direction = Left 
               | Up
               | Right
               | Down

type Msg = 
  {- navigation and editing -}
    SetCursor (Maybe Char) (Maybe Direction)
  | SwapCursor
  | MoveCursor Direction
  | SelectIndex Cursor

  {- saving and loading -}
  | RequestPuzzleFile
  | ReceivedPuzzleFile File
  | LoadedPuzzleFile String

  {- UI business -}
  | VisibilityChanged Browser.Events.Visibility
  | Focused (Result Browser.Dom.Error ())

port savePlayedPuzzles : Encode.Value -> Cmd msg

type Cursor = Board Int
            | Clues Int Int

defaultCursor : Cursor
defaultCursor = Clues 0 0

type alias Clue = Puzzle.BlankClue
type alias Puzzle = Puzzle.Blank

type alias Model =
    { mode : Mode
    , msg : Maybe String
    , focused : Bool
    , playedPuzzles : List GameState
    }

type Mode = NoPuzzle
          | Playing GameState

type alias GameState = 
    { cursor : Cursor
    , puzzle : Puzzle
    }

gameStateEncoder : GameState -> Encode.Value
gameStateEncoder gs =
    Encode.object
        [ ("cursor", cursorEncoder gs.cursor)
        , ("puzzle", Puzzle.encodeBlank gs.puzzle)
        ]

cursorEncoder : Cursor -> Encode.Value
cursorEncoder cursor =
    case cursor of
        Board index -> Encode.object [("boardIndex", Encode.int index)]
        Clues cIndex lIndex -> 
            Encode.object [ ("clueIndex", Encode.int cIndex)
                          , ("letterIndex", Encode.int lIndex)
                          ]

gameStateDecoder : Decode.Decoder GameState
gameStateDecoder =
    Decode.oneOf
        [ Decode.map2
              (\cursor puzzle ->
                   { cursor = cursor
                   , puzzle = puzzle
                   })
              (Decode.oneOf 
                   [ Decode.field "cursor" cursorDecoder
                   , Decode.null defaultCursor
                   ])
              (Decode.field "puzzle" Puzzle.blankDecoder)
        , Decode.map
            (\puzzle ->
                 { cursor = defaultCursor
                 , puzzle = puzzle
                 })
            Puzzle.blankDecoder
        ]

cursorDecoder : Decode.Decoder Cursor
cursorDecoder =
    Decode.oneOf
        [ Decode.map Board 
              (Decode.field "boardIndex" Decode.int)
        , Decode.map2 Clues 
            (Decode.field "clueIndex" Decode.int)
            (Decode.field "letterIndex" Decode.int)
        ]

defaultModel : Model
defaultModel = 
    { mode = NoPuzzle
    , msg = Nothing
    , focused = False
    , playedPuzzles = []
    }

withPlayedPuzzles : List GameState -> Model -> Model
withPlayedPuzzles playedPuzzles model = { model | playedPuzzles = playedPuzzles }

trySaveCurrentPuzzle : Model -> Model
trySaveCurrentPuzzle model =
    case model.mode of
        NoPuzzle -> model
        Playing old -> model |>
                       withPlayedPuzzles (old::model.playedPuzzles)

clearMsg : Model -> Model
clearMsg model = { model | msg = Nothing }

withMsg : String -> Model -> Model
withMsg msg model = { model | msg = Just msg }

withFocused : Bool -> Model -> Model
withFocused focused model = { model | focused = focused }

asModeIn : Model -> Mode -> Model
asModeIn model mode = { model | mode = mode }

asPuzzleIn : GameState -> Puzzle -> GameState
asPuzzleIn state puzzle = { state | puzzle = puzzle }

withCursor : Cursor -> GameState -> GameState
withCursor cursor state = { state | cursor = cursor }

swapCursor : GameState -> GameState
swapCursor state =
    (\c -> withCursor c state) <|
    case state.cursor of
        Board _ -> 
            let (cIndex, lIndex) = selectedClue state in
            Clues cIndex lIndex
        Clues _ _ -> Board (selectedBoard state)
            
moveCursor : Direction -> GameState -> GameState
moveCursor dir state =
    (\c -> withCursor c state) <|
    case state.cursor of
        Board index -> 
            let 
                squares = boardSquares state.puzzle

                selected = squares |>
                           List.filter
                               (\square -> 
                                    case square.square of
                                        Black -> False
                                        White sqIndex _ -> index == sqIndex) |>
                           List.head |> {- FIXME YIKES -}
                           Maybe.withDefault { square = Black, row = 0, col = 0 }

                aligned = squares |>
                          List.filter
                              (\square -> 
                                   remainderBy state.puzzle.boardColumns square.col == 
                                   remainderBy state.puzzle.boardColumns selected.col)
            in

            case dir of 
                Left -> Board <| Basics.max 0 <| index - 1
                Right -> Board <| Basics.min (List.length state.puzzle.quote) <| index + 1
                Up -> aligned |>
                      List.filter (\square -> square.row < selected.row) |>
                      List.sortBy .row |>
                      List.reverse |>
                      List.filterMap (\square -> 
                                          case square.square of
                                              Black -> Nothing
                                              White qIndex _ -> Just qIndex) |>
                      List.head |>
                      Maybe.withDefault index |>
                      Board
                Down -> aligned |>
                      List.filter (\square -> square.row > selected.row) |>
                      List.sortBy .row |>
                      List.filterMap (\square -> 
                                          case square.square of
                                              Black -> Nothing
                                              White qIndex _ -> Just qIndex) |>
                      List.head |>
                      Maybe.withDefault index |>
                      Board

        Clues cIndex lIndex -> 
            case dir of
                Left -> Clues cIndex (Basics.max (lIndex - 1) 0)
                Up -> Clues (Basics.max (cIndex - 1) 0) 0
                Down -> Clues (Basics.min 
                                   (cIndex + 1) 
                                   (List.length state.puzzle.clues - 1)) 
                              0
                Right -> Clues cIndex (Basics.min 
                                           (lIndex + 1) 
                                           (state.puzzle.clues |>
                                            List.drop cIndex |>
                                            List.head |>
                                            Maybe.map (.answer >> List.length >> (\n -> n - 1)) |>
                                            Maybe.withDefault lIndex))

type SelectionMode = NotSelected | AsClue | AsBoard
                        
isSelected : Int -> (Int, Int) -> GameState -> SelectionMode
isSelected qIndex (cIndex, lIndex) state =
    case state.cursor of
        Clues clue letter -> if clue == cIndex && letter == lIndex then AsClue else NotSelected
        Board quote -> if quote == qIndex then AsBoard else NotSelected

selectedClue : GameState -> (Int, Int)
selectedClue state =
    case state.cursor of
        Clues clue letter -> (clue, letter)
        Board index ->
            state.puzzle.clues |>
            List.indexedMap
                (\clueIndex clue ->
                     clue.answer |>
                     List.indexedMap 
                         (\letterIndex qIndex -> 
                              ((clueIndex, letterIndex), qIndex == index)) |>
                     List.filter Tuple.second |>
                     List.map Tuple.first) |>
            List.concat |>
            (\cs ->
                 case cs of
                     [idx] -> idx
                     _ -> (-1, -1) {- YIKES -})

selectedBoard : GameState -> Int
selectedBoard state =
    case state.cursor of
        Board index -> index
        Clues clueIndex letterIndex ->
            state.puzzle.clues |>
            List.drop clueIndex |>
            List.head |>
            Maybe.andThen
                (\clue ->
                     clue.answer |>
                     List.drop letterIndex |>
                     List.head) |>
            Maybe.withDefault (-1) {- YIKES -}

andSave : Model -> (Model, Cmd Msg)
andSave model = 
    ( model
    , savePlayedPuzzles <| 
        Encode.list gameStateEncoder <|
        case model.mode of
            NoPuzzle -> model.playedPuzzles
            Playing gs -> gs::model.playedPuzzles)

-- MAIN

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

-- INIT/SUBSCRIPTIONS

getFocus : Cmd Msg
getFocus = Task.attempt Focused (Browser.Dom.focus "crossbars-wrapper")

saveDecoder : Decode.Decoder { playedPuzzles : List GameState
                             , url : String
                             }
saveDecoder = 
    Decode.map2 
        (\playedPuzzles url ->
             { playedPuzzles = playedPuzzles
             , url = url
             })
        (Decode.field "playedPuzzles" (Decode.oneOf
                                           [ Decode.list gameStateDecoder
                                           , Decode.null []
                                           ]))
        (Decode.field "url" Decode.string)

tryLoadRecentPuzzle : Model -> Model
tryLoadRecentPuzzle model =
    case model.playedPuzzles of
        recent::rest ->
            recent |> 
            Playing |> 
            asModeIn model |>
            withPlayedPuzzles rest
        _ -> model

selectPuzzle : String -> Model -> Model
selectPuzzle sUrl model =
    Url.fromString sUrl |>
    Maybe.andThen .query |> 
    Maybe.andThen Url.percentDecode |>
    Maybe.andThen (Decode.decodeString gameStateDecoder >>
                   Result.toMaybe) |> Debug.log "decoded" |>
    \mPuzzle -> case mPuzzle of
        Nothing -> tryLoadRecentPuzzle model
        Just puzzle -> puzzle |>
                       Playing |>
                       asModeIn defaultModel

init : Flags -> (Model, Cmd Msg)
init json =
    let model =
            case Decode.decodeValue saveDecoder json of
                Err _ -> defaultModel
                Ok saved -> defaultModel |>
                            withPlayedPuzzles saved.playedPuzzles |>
                            selectPuzzle saved.url
    in
        ( model |> Debug.log "initial model"
        , getFocus
        )

msgOfKey : Decode.Decoder (Msg, Bool)
msgOfKey =
    Decode.map4
        (\alt ctrl meta key ->
             let keyDesc = (if ctrl then "C-" else "") ++ 
                           (if meta then "M-" else "") ++ 
                           (if alt  then "A-" else "") ++ 
                           (key |> Debug.log "key")
             in
                 case (alt || ctrl || meta, String.uncons key, key) of
                     (modified, Just (c, ""), _) ->
                         if ctrl && not (alt || meta) && c == 'd'
                         then Ok <| SetCursor Nothing Nothing
                         else if not modified && Char.isAlphaNum c
                         then Ok <| SetCursor (Just <| Char.toUpper c) (Just Right)
                         else if not modified && c == ' '
                         then Ok <| SetCursor Nothing Nothing
                         else Err keyDesc
                     (False, _, "Tab") -> Ok SwapCursor
                     (False, _, "Backspace") -> Ok <| SetCursor Nothing (Just Left)
                     (False, _, "Delete") -> Ok <| SetCursor Nothing Nothing
                     (False, _, "Del") -> Ok <| SetCursor Nothing Nothing
                     (False, _, "Clear") -> Ok <| SetCursor Nothing Nothing
                     (False, _, "ArrowLeft") -> Ok <| MoveCursor Left
                     (False, _, "ArrowUp") -> Ok <| MoveCursor Up
                     (False, _, "ArrowRight") -> Ok <| MoveCursor Right
                     (False, _, "ArrowDown") -> Ok <| MoveCursor Down
                     (_, _, _) -> Err keyDesc)
            (Decode.field "altKey" Decode.bool)
            (Decode.field "ctrlKey" Decode.bool)
            (Decode.field "metaKey" Decode.bool)
            (Decode.field "key" Decode.string) |>
    Decode.andThen
        (\mmsg ->
             case mmsg of
                 Err key -> Decode.fail ("ignoring " ++ key)
                 Ok msg -> Decode.succeed (msg, True))

subscriptions : Model -> Sub Msg
subscriptions model = 
    Browser.Events.onVisibilityChange VisibilityChanged

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model.mode, msg) of
        (_, RequestPuzzleFile) ->
            ( model
            , Select.file ["application/json", "text/plain"] ReceivedPuzzleFile)

        (_, ReceivedPuzzleFile file) ->
            ( model
            , Task.perform LoadedPuzzleFile (File.toString file))

        (_, LoadedPuzzleFile cts) ->
            andSave <|
                case Decode.decodeString gameStateDecoder cts of
                    Err err -> model |> 
                               withMsg "Couldn't load file." 
                               
                    Ok gs -> 
                        case Puzzle.isValidBlank gs.puzzle of
                        [] -> gs |>
                              Playing |>
                              asModeIn model |>
                              clearMsg |>
                              trySaveCurrentPuzzle
                        problems -> 
                            model |>
                            withMsg ("Invalid puzzle: " ++ 
                                         String.join ", " 
                                         (List.map Puzzle.problemToString problems))

        (_, Focused (Err _)) -> 
            ( model |> 
              withFocused False
            , Cmd.none)

        (_, Focused (Ok ())) ->
            ( model |>
              withFocused True
            , Cmd.none)

        (_, VisibilityChanged vis) ->
            ( model
            , if vis == Browser.Events.Visible
              then getFocus
              else Cmd.none)

        (NoPuzzle, _) -> 
            ( model
            , Cmd.none)

        (Playing state, SetCursor mc mdir) -> 
            updateIndex (selectedBoard state) (always mc) state.puzzle.quote |>
            Puzzle.asQuoteIn state.puzzle |>
            asPuzzleIn state |>
            (case mdir of
                 Nothing -> identity
                 Just dir -> moveCursor dir) |>
            Playing |>
            asModeIn model |>
            withFocused True |>
            andSave
                               
        (Playing state, SwapCursor) -> 
            state |>
            swapCursor |>
            Playing |>
            asModeIn model |>
            withFocused True |>
            andSave
                          
        (Playing state, MoveCursor dir) -> 
            state |>
            moveCursor dir |>
            Playing  |>
            asModeIn model |>
            withFocused True |>
            andSave
     
        (Playing state, SelectIndex cursor) -> 
            state |>
            withCursor cursor |>
            Playing |>
            asModeIn model |>
            andSave

-- VIEW

view : Model -> Html Msg
view model =
    div [ id "crossbars-wrapper"
        , tabindex 0
        , Html.Events.preventDefaultOn "keydown" msgOfKey
        , onFocus (Focused (Ok ()))
        ] 
        (  section [id "overview"]
           [ h3 [class "header"] [text "Crossbars — Acrostic Player"]
           , div [id "saved"]
               [ input [ type_ "button" 
                       , id "loadpuzzle"
                       , value "Load puzzle from file..."
                       , onClick RequestPuzzleFile
                       ]
                     []     
               ]
           , div [id "warnings"]
               (let

                   focusWarning =
                       if model.focused 
                       then [] 
                       else ["If keyboard controls do not work, try clicking in the play area."]

                   otherWarnings = 
                       case model.msg of
                           Nothing -> []
                           Just msg -> [msg]

                in

                    (focusWarning ++ otherWarnings) |>
                    List.map
                        (text >> List.singleton >> span [class "warning"]))
           ]
        :: case model.mode of
               NoPuzzle -> [ ]
               Playing state -> playingView state
        )

playingView : GameState -> List (Html Msg)
playingView state = 
    [ section [id "board-wrapper"]
          [ boardView state ]
    , section [id "clues"]
        [ h3 [class "header"] [text "Clues"]
        , state.puzzle.clues |>
          List.indexedMap (clueView state) |>
          div [id "clue-list"]
        ]
    , section [id "help"]
        [ h3 [ class "header" ] [text "How to use the acrostic player"]
        , p [] [ text "Click to select a square on the board or in clues. Good luck!"]
        , h4 [] [text "Keyboard shortcuts"]
        , div [ id "kbd-help" ] 
            [ span [] [text "TAB - switch between board and clues"]
            , span [] [text "ARROW KEYS - navigate"]
            , span [] [text "BACKSPACE - delete current entry (and move back one)"]
            , span [] [text "DELETE, Ctrl-D, SPACEBAR - delete current entry (and do not move)"]
            ]
        ]
    ]

type UnnumberedSquare = White Int (Maybe Char) | Black

type alias Square = 
    { square : UnnumberedSquare
    , row : Int
    , col : Int
    }

boardSquares : Puzzle -> List Square
boardSquares puzzle =
    let         

        indexedQuote = List.indexedMap Tuple.pair puzzle.quote
                  
        squares = 
            let collect quote lens =
                    case lens of
                        [] -> [] {- FIXME quote had better be empty---yikes! -}
                        len::rest ->
                            List.map (\(idx, mc) -> White idx mc) (List.take len quote) ++
                            Black :: 
                            collect (List.drop len quote) rest
            in
                collect indexedQuote puzzle.quoteWordLengths

        numberedSquares =
            let number idx count l =
                    let row = count // puzzle.boardColumns in
                    case l of
                        [] -> List.range (remainderBy puzzle.boardColumns count) (puzzle.boardColumns - 1) |>
                              List.map
                                  (\col -> { square = Black
                                           , col = col
                                           , row = row
                                           })
                        (sq::rest) ->
                            { square = sq
                            , col = remainderBy puzzle.boardColumns count
                            , row = row
                            } :: number (idx + if sq == Black then 0 else 1) (count + 1) rest
            in
                number 0 0 squares

    in

        numberedSquares

boardView : GameState -> Html Msg
boardView state =
    let 

        uses = state.puzzle.clues |>
               List.indexedMap 
                   (\cIndex clue ->
                        clue.answer |> List.map (\qIndex -> (qIndex, cIndex))) |>
               List.concat |>
               Dict.fromList

        width = 360

        numCols = state.puzzle.boardColumns

        numberedSquares = boardSquares state.puzzle

        numSquares = List.length numberedSquares

        boxWidth = width / toFloat numCols

        numRows = (numSquares // numCols)
                
        height = (toFloat numRows) * boxWidth

        selection = { index = selectedBoard state
                    , fg = case state.cursor of
                               Board _ -> True
                               Clues _ _ -> False
                    }

        orderedSquares =
            let (selected,unselected) = 
                    numberedSquares |>
                    List.partition
                        (\square ->
                             case square.square of
                                 Black -> False
                                 White index _ -> index == selection.index)
            in
                unselected ++ selected

    in

    Svg.svg [ id "board"
            , Svg.Attributes.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height) ] 
        (orderedSquares |>
         List.map
             (\square ->
                  let x = toFloat square.col * boxWidth
                      y = toFloat square.row * boxWidth
                      thirdBox = boxWidth / 3
                      textLength = thirdBox |> String.fromFloat
                      box clss = Svg.rect 
                                 ( List.map (Svg.Attributes.class) clss ++
                                   [ x |> String.fromFloat |> Svg.Attributes.x
                                   , y |> String.fromFloat |> Svg.Attributes.y
                                   , boxWidth |> String.fromFloat |> Svg.Attributes.width
                                   , boxWidth |> String.fromFloat |> Svg.Attributes.height 
                                   , Svg.Attributes.class "board-square" 
                                   ])
                                []
                  in

                      case square.square of
                          Black -> box ["board-black"]
                          White index mc ->
                              let
                                  usedIn = Dict.get index uses |>
                                           Maybe.withDefault (-1) {- FIXME yikes -}
                              in

                              Svg.g 
                                  [ onClick (SelectIndex (Board index)) ]
                                  [ box ( "board-white" :: 
                                          if selection.index == index
                                          then if selection.fg 
                                               then ["selected"]
                                               else ["selected-bg"]
                                          else [])
                                  , Svg.text_
                                       [ x + 1 |> String.fromFloat |> Svg.Attributes.x
                                       , y + thirdBox |> String.fromFloat |> Svg.Attributes.y
                                       , Svg.Attributes.textAnchor "start"
                                       , Svg.Attributes.class "number"
                                       ]
                                       [ index + 1 |> String.fromInt |> Svg.text ]
                                 , Svg.text_
                                       [ x + boxWidth - 1 |> String.fromFloat |> Svg.Attributes.x
                                       , y + thirdBox |> String.fromFloat |> Svg.Attributes.y
                                       , Svg.Attributes.textAnchor "end"
                                       , Svg.Attributes.class "clue-letter"
                                       ]
                                       [ usedIn |> Puzzle.letterFor |> Svg.text ]
                                 , Svg.text_
                                       [ x + (boxWidth / 2) |> String.fromFloat |> Svg.Attributes.x
                                       , y + boxWidth - 1.5 |> String.fromFloat |> Svg.Attributes.y
                                       , Svg.Attributes.textAnchor "middle"
                                       , Svg.Attributes.class "letter"
                                       ]
                                       [ mc |> Maybe.withDefault ' ' |> String.fromChar |> Svg.text ]
                                  ]))
             

clueView : GameState -> Int -> Clue -> Html Msg
clueView state clueIndex clue =
    let 
        letter = Puzzle.letterFor clueIndex 
    in

    div [ class "clue", 
          id ("clue-" ++ String.fromInt clueIndex)
        ]
        [ h3 [ class "hint" ] 
             [ span [ class "clue-letter" ] [ text (letter ++ ". ") ]
             , span [ class "hint-text" ] [ text clue.hint ] {- FIXME support markdown -}
             ]
        , table []
            [ tr []
                 (clue.answer |>
                  List.indexedMap 
                      (\letterIndex quoteIndex ->
                           td [ onClick (SelectIndex (Clues clueIndex letterIndex)) 
                              , classList
                                    [ ("answer-letter", True)
                                    , ("selected", isSelected quoteIndex (clueIndex, letterIndex) state == AsClue)
                                    , ("selected-bg", isSelected quoteIndex (clueIndex, letterIndex) state == AsBoard)
                                    ]
                              ]
                              [ List.drop quoteIndex state.puzzle.quote |>
                                List.head |>
                                Maybe.andThen identity |> 
                                Maybe.withDefault ' ' |>
                                String.fromChar |>
                                text
                              ]))
            , tr []
                 (clue.answer |>
                  List.indexedMap 
                      (\letterIndex quoteIndex ->
                           td [ onClick (SelectIndex (Clues clueIndex letterIndex)) 
                              , classList
                                    [ ("answer-number", True)
                                    , ("selected", isSelected quoteIndex (clueIndex, letterIndex) state == AsClue)
                                    , ("selected-bg", isSelected quoteIndex (clueIndex, letterIndex) state == AsBoard)
                                    ]
                              ]
                              [ quoteIndex + 1 |> 
                                String.fromInt |>
                                text
                              ]))
            ]
        ]
