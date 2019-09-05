port module Main exposing (..)

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)

import Browser

import Json.Encode

import Puzzle

-- TYPES

type alias Flags = Json.Encode.Value

type Msg = SetCursor (Maybe Char)
         | SelectIndex Cursor

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

withCursor : Cursor -> State -> State
withCursor cursor state = { state | cursor = cursor }

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
    , Cmd.none
    )

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
                SetCursor mc -> Debug.todo "SetCursor"
                                        
                SelectIndex cursor -> 
                    ( state |>
                      withCursor cursor |>
                      Playing
                    , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div [id "crossbars-wrapper"] 
        (  section [id "overview"]
           [ h3 [class "header"] [text "Crossbars — Acrostic Player"]
           ]
        :: case model of
               NoPuzzle -> [ text "No puzzle loaded... 😦" ]
               Playing state -> playingView state
        )
              

playingView : State -> List (Html Msg)
playingView state = 
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
                                     (\letterIndex (quoteIndex, mc) ->
                                          td [ onClick (SelectIndex (Clues clueIndex letterIndex)) 
                                             , class "answer-letter"
                                             ]
                                             [ mc |> 
                                               Maybe.withDefault ' ' |> 
                                               String.fromChar |>
                                               text
                                             ]))
                           , tr []
                                (clue.answer |>
                                 List.indexedMap 
                                     (\letterIndex (quoteIndex, mc) ->
                                          td [ onClick (SelectIndex (Clues clueIndex letterIndex)) 
                                             , class "answer-number"
                                             ]
                                             [ quoteIndex + 1 |> 
                                               String.fromInt |>
                                               text
                                             ]))
                           ]
                       ]) |>
                  div [id "clue-list"]
            ]
        ]

-- TESTING

wcw : Puzzle
wcw = { boardColumns = 35, clues = [{ answer = [(150,Nothing),(202,Nothing),(16,Nothing),(71,Nothing),(112,Nothing),(209,Nothing),(196,Nothing),(88,Nothing),(213,Nothing),(27,Nothing),(130,Nothing)], hint = "Red object glazed with rainwater in a poem by this quote's author" },{ answer = [(44,Nothing),(121,Nothing),(38,Nothing),(155,Nothing),(54,Nothing),(197,Nothing),(70,Nothing)], hint = "Cold compress (2 wds.)" },{ answer = [(210,Nothing),(157,Nothing),(138,Nothing),(169,Nothing),(187,Nothing)], hint = "In bounds" },{ answer = [(20,Nothing),(179,Nothing),(174,Nothing),(30,Nothing),(90,Nothing),(81,Nothing),(37,Nothing),(139,Nothing),(74,Nothing)], hint = "Unexpected gift, in Louisiana Creole French" },{ answer = [(57,Nothing),(124,Nothing),(167,Nothing),(18,Nothing),(98,Nothing),(105,Nothing),(176,Nothing),(164,Nothing),(97,Nothing),(127,Nothing),(67,Nothing),(50,Nothing),(211,Nothing)], hint = "\"Great men are over-estimated and small men are _______\", George Eliot, _Adam Bede_" },{ answer = [(203,Nothing),(153,Nothing),(92,Nothing),(1,Nothing),(40,Nothing),(168,Nothing),(99,Nothing),(58,Nothing),(80,Nothing)], hint = "Recipient of the Pritzker Prize" },{ answer = [(166,Nothing),(116,Nothing),(17,Nothing),(119,Nothing),(133,Nothing),(200,Nothing),(41,Nothing),(186,Nothing),(31,Nothing),(198,Nothing),(4,Nothing),(45,Nothing),(0,Nothing),(60,Nothing)], hint = "A spoonful of sugar helps overcome it (2 wds.)" },{ answer = [(219,Nothing),(191,Nothing),(95,Nothing),(193,Nothing),(103,Nothing),(149,Nothing),(36,Nothing)], hint = "It means the same thing" },{ answer = [(132,Nothing),(68,Nothing),(52,Nothing),(220,Nothing)], hint = "Raises hackles" },{ answer = [(160,Nothing),(205,Nothing),(42,Nothing)], hint = "Yuletide flip, for short" },{ answer = [(64,Nothing),(15,Nothing),(53,Nothing),(177,Nothing),(154,Nothing),(215,Nothing),(192,Nothing),(118,Nothing),(73,Nothing),(5,Nothing)], hint = "\"No defeat is made up entirely of defeat\" poem by this quote's author (2 wds.)" },{ answer = [(134,Nothing),(125,Nothing),(161,Nothing),(143,Nothing),(10,Nothing),(190,Nothing),(14,Nothing),(165,Nothing),(201,Nothing)], hint = "\"_______ Venus\", racist exhibition of 19th Century Europe" },{ answer = [(185,Nothing),(173,Nothing),(91,Nothing),(216,Nothing),(156,Nothing),(135,Nothing),(140,Nothing),(19,Nothing)], hint = "Spellbind" },{ answer = [(189,Nothing),(48,Nothing),(76,Nothing),(162,Nothing),(85,Nothing),(151,Nothing),(144,Nothing),(194,Nothing),(96,Nothing),(120,Nothing)], hint = "Questions doubters (3 wds.)" },{ answer = [(129,Nothing),(145,Nothing),(12,Nothing),(108,Nothing),(217,Nothing),(183,Nothing),(207,Nothing),(34,Nothing)], hint = "Lamb-like quality" },{ answer = [(141,Nothing),(184,Nothing),(29,Nothing),(55,Nothing),(122,Nothing),(49,Nothing),(63,Nothing),(146,Nothing),(87,Nothing)], hint = "Vividly remniscent" },{ answer = [(148,Nothing),(56,Nothing),(113,Nothing),(180,Nothing),(86,Nothing),(83,Nothing),(35,Nothing),(75,Nothing)], hint = "Like a pulse" },{ answer = [(128,Nothing),(123,Nothing),(175,Nothing),(178,Nothing),(8,Nothing),(171,Nothing),(82,Nothing),(2,Nothing),(28,Nothing)], hint = "Grill on camera?" },{ answer = [(100,Nothing),(69,Nothing),(163,Nothing),(3,Nothing),(214,Nothing),(159,Nothing),(206,Nothing),(136,Nothing),(66,Nothing),(59,Nothing),(21,Nothing)], hint = "Frappuccino, Coolatta, or Awful Awful (2 wds.)" },{ answer = [(33,Nothing),(204,Nothing),(93,Nothing),(6,Nothing),(115,Nothing),(89,Nothing),(13,Nothing),(51,Nothing),(107,Nothing)], hint = "Couldn't care less" },{ answer = [(111,Nothing),(147,Nothing),(23,Nothing),(109,Nothing),(199,Nothing),(137,Nothing),(26,Nothing),(77,Nothing),(131,Nothing),(32,Nothing)], hint = "Fit to print" },{ answer = [(46,Nothing),(39,Nothing),(24,Nothing),(25,Nothing)], hint = "\"Mad Dog\" Maddux, familiarly" },{ answer = [(117,Nothing),(47,Nothing),(195,Nothing),(65,Nothing),(106,Nothing),(170,Nothing),(212,Nothing),(110,Nothing),(182,Nothing),(104,Nothing)], hint = "Birthplace and hometown of this quote's author" },{ answer = [(79,Nothing),(9,Nothing),(101,Nothing),(62,Nothing),(102,Nothing),(22,Nothing),(43,Nothing),(61,Nothing),(152,Nothing),(84,Nothing),(188,Nothing)], hint = "Adolescent quality, often" },{ answer = [(208,Nothing),(142,Nothing),(158,Nothing),(7,Nothing),(72,Nothing),(126,Nothing)], hint = "Devise; lie" },{ answer = [(94,Nothing),(218,Nothing),(114,Nothing),(78,Nothing),(172,Nothing),(11,Nothing),(181,Nothing)], hint = "Record breaker (2 wds.)" }], quote = [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing], quoteWordLengths = [3,6,2,3,3,6,2,4,4,1,9,7,4,1,7,4,1,6,4,4,4,5,2,6,4,3,5,4,5,2,6,3,5,1,9,10,8,4,8,2,5,7,3,7,4,1,8,9] }
