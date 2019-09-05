port module Main exposing (..)

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)

import Browser
import Browser.Events
import Browser.Dom

import Json.Decode as Decode
import Json.Encode as Encode

import Task

import Puzzle
import Util exposing (..)

-- TYPES

type alias Flags = Encode.Value

type Direction = Left 
               | Up
               | Right
               | Down

type Msg = SetCursor (Maybe Char) (Maybe Direction)
         | SwapCursor
         | MoveCursor Direction
         | SelectIndex Cursor
         | Focused (Result Browser.Dom.Error ())
         | IgnoreKey String

type Cursor = Board Int
            | Clues Int Int

type alias Clue = Puzzle.BlankClue
type alias Puzzle = Puzzle.Blank

type Model = NoPuzzle
           | Playing State

type alias State = 
    { cursor : Cursor
    , puzzle : Puzzle
    }

defaultModel : Model
defaultModel = NoPuzzle

testModel : Model
testModel = Playing { cursor = Clues 0 0 
                    , puzzle = wcw
                    }

asPuzzleIn : State -> Puzzle -> State
asPuzzleIn state puzzle = { state | puzzle = puzzle }

withCursor : Cursor -> State -> State
withCursor cursor state = { state | cursor = cursor }

swapCursor : State -> State
swapCursor state =
    (\c -> withCursor c state) <|
    case state.cursor of
        Board _ -> 
            let (cIndex, lIndex) = selectedClue state in
            Clues cIndex lIndex
        Clues _ _ -> Board (selectedBoard state)
            
moveCursor : Direction -> State -> State
moveCursor dir state =
    (\c -> withCursor c state) <|
    case state.cursor of
        Board index -> Debug.todo "moveCursor Board"
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
                        
isSelected : Int -> (Int, Int) -> State -> SelectionMode
isSelected qIndex (cIndex, lIndex) state =
    case state.cursor of
        Clues clue letter -> if clue == cIndex && letter == lIndex then AsClue else NotSelected
        Board quote -> if quote == qIndex then AsBoard else NotSelected

selectedClue : State -> (Int, Int)
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

selectedBoard : State -> Int
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

-- MAIN

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

-- INIT/SUBSCRIPTIONS

init : Flags -> (Model, Cmd Msg)
init savedModel =
    ( testModel
    , Task.attempt Focused (Browser.Dom.focus "crossbars-wrapper")
    )

msgOfKey : Decode.Decoder (Msg, Bool)
msgOfKey =
    Decode.map4
        (\alt ctrl meta key ->
             let keyDesc = (if ctrl then "C-" else "") ++ 
                           (if meta then "M-" else "") ++ 
                           (if alt  then "A-" else "") ++ 
                           key
             in
                 case (alt || ctrl || meta, String.uncons key, key) of
                     (modified, Just (c, ""), _) ->
                         if ctrl && not (alt || meta) && c == 'd'
                         then Ok <| SetCursor Nothing Nothing
                         else if not modified && Char.isAlphaNum c
                         then Ok <| SetCursor (Just <| Char.toUpper c) (Just Right)
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
subscriptions model = Sub.none

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        NoPuzzle -> ( model
                    , Cmd.none)
        Playing state ->
            case msg |> Debug.log "msg" of
                SetCursor mc mdir -> 
                    ( updateIndex (selectedBoard state) (always mc) state.puzzle.quote |>
                      Puzzle.asQuoteIn state.puzzle |>
                      asPuzzleIn state |>
                      (case mdir of
                           Nothing -> identity
                           Just dir -> moveCursor dir) |>
                      Playing
                    , Cmd.none)
                                   
                SwapCursor -> 
                    ( state |>
                      swapCursor |>
                      Playing
                    , Cmd.none)
                              
                MoveCursor dir -> 
                    ( state |>
                      moveCursor dir |>
                      Playing
                    , Cmd.none)
     
                SelectIndex cursor -> 
                    ( state |>
                      withCursor cursor |>
                      Playing
                    , Cmd.none)

                Focused _ -> ( state |>
                               Playing {- FIXME warning that keyboard controls won't work -}
                             , Cmd.none)

                IgnoreKey _ -> ( state |>
                                 Playing
                               , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div [ id "crossbars-wrapper"
        , tabindex 0
        , Html.Events.preventDefaultOn "keydown" msgOfKey
        ] 
        (  section [id "overview"]
           [ h3 [class "header"] [text "Crossbars — Acrostic Player"]
           ]
        :: case model of
               NoPuzzle -> [ text "No puzzle loaded... 😦" ]
               Playing state -> playingView state
        )
              

playingView : State -> List (Html Msg)
playingView state = 
    let 
        selected = selectedClue state
    in

    [ section [id "board"]
          []
    , section [id "clues"]
        [ h3 [class "header"] [text "Clues"]
        , state.puzzle.clues |>
          List.indexedMap
              (\clueIndex clue ->
                   let 
                       letter = Puzzle.letterFor clueIndex 
                   in

                   div [ class "clue", 
                         id ("clue-" ++ String.fromInt clueIndex)
                       ]
                       [ h3 [ class "hint" ] 
                            [ span [class "clue-letter"] [ text (letter ++ ". ") ]
                            , text clue.hint {- FIXME support markdown -}
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
                       ]) |>
                  div [id "clue-list"]
            ]
    , section [id "help"]
        [ h3 [ class "header" ] [text "How to use the acrostic player"]
        , p [] [ text "Click to select a square on the board or in clues. Good luck!"]
        , h4 [] [text "Keyboard shortcuts"]
        , ul [] 
            [ li [] [text "TAB - switch between board and clues"]
            , li [] [text "ARROW KEYS - navigate"]
            , li [] [text "BACKSPACE - delete current entry (and move back one)"]
            , li [] [text "DELETE, Ctrl-D - delete currentry (and do not move)"]
            ]
        ]
    ]

-- TESTING

wcw : Puzzle
wcw = { boardColumns = 35, clues = [{ answer = [150,202,16,71,112,209,196,88,213,27,130], hint = "Red object glazed with rainwater in a poem by this quote's author" },{ answer = [44,121,38,155,54,197,70], hint = "Cold compress (2 wds.)" },{ answer = [210,157,138,169,187], hint = "In bounds" },{ answer = [20,179,174,30,90,81,37,139,74], hint = "Unexpected gift, in Louisiana Creole French" },{ answer = [57,124,167,18,98,105,176,164,97,127,67,50,211], hint = "\"Great men are over-estimated and small men are _______\", George Eliot, _Adam Bede_" },{ answer = [203,153,92,1,40,168,99,58,80], hint = "Recipient of the Pritzker Prize" },{ answer = [166,116,17,119,133,200,41,186,31,198,4,45,0,60], hint = "A spoonful of sugar helps overcome it (2 wds.)" },{ answer = [219,191,95,193,103,149,36], hint = "It means the same thing" },{ answer = [132,68,52,220], hint = "Raises hackles" },{ answer = [160,205,42], hint = "Yuletide flip, for short" },{ answer = [64,15,53,177,154,215,192,118,73,5], hint = "\"No defeat is made up entirely of defeat\" poem by this quote's author (2 wds.)" },{ answer = [134,125,161,143,10,190,14,165,201], hint = "\"_______ Venus\", racist exhibition of 19th Century Europe" },{ answer = [185,173,91,216,156,135,140,19], hint = "Spellbind" },{ answer = [189,48,76,162,85,151,144,194,96,120], hint = "Questions doubters (3 wds.)" },{ answer = [129,145,12,108,217,183,207,34], hint = "Lamb-like quality" },{ answer = [141,184,29,55,122,49,63,146,87], hint = "Vividly remniscent" },{ answer = [148,56,113,180,86,83,35,75], hint = "Like a pulse" },{ answer = [128,123,175,178,8,171,82,2,28], hint = "Grill on camera?" },{ answer = [100,69,163,3,214,159,206,136,66,59,21], hint = "Frappuccino, Coolatta, or Awful Awful (2 wds.)" },{ answer = [33,204,93,6,115,89,13,51,107], hint = "Couldn't care less" },{ answer = [111,147,23,109,199,137,26,77,131,32], hint = "Fit to print" },{ answer = [46,39,24,25], hint = "\"Mad Dog\" Maddux, familiarly" },{ answer = [117,47,195,65,106,170,212,110,182,104], hint = "Birthplace and hometown of this quote's author" },{ answer = [79,9,101,62,102,22,43,61,152,84,188], hint = "Adolescent quality, often" },{ answer = [208,142,158,7,72,126], hint = "Devise; lie" },{ answer = [94,218,114,78,172,11,181], hint = "Record breaker (2 wds.)" }], quote = [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing], quoteWordLengths = [3,6,2,3,3,6,2,4,4,1,9,7,4,1,7,4,1,6,4,4,4,5,2,6,4,3,5,4,5,2,6,3,5,1,9,10,8,4,8,2,5,7,3,7,4,1,8,9] }