port module Main exposing (..)

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)

import Browser

import Json.Encode

-- TYPES

type alias Flags = Json.Encode.Value

type Msg = UpdateIndex Int (Maybe Char)
         | SelectIndex Cursor

type Cursor = Board { index : Int}
            | Clues { clueIndex : Int 
                    , letterIndex : Int
                    }

type alias Clue =
    { hint : String
    , answer : List (Int, Maybe Char)
    }

type alias Puzzle =
    { quote : List (Maybe Char)
    , quoteWordLengths : List Int
    , boardColumns : Int
    , clues : List Clue
    }

type Model = NoPuzzle
           | Playing State

type alias State = 
    { cursor : Cursor
    , puzzle : Puzzle
    }

defaultModel : Model
defaultModel = NoPuzzle

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
    ( defaultModel
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
            case msg of
                UpdateIndex index mc -> Debug.todo "UpdateIndex"
                                        
                SelectIndex cursor -> 
                    ( state |>
                      withCursor cursor |>
                      Playing
                    , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = 
    div [id "crossbars-wrapper"] 
        [ section [id "overview"]
          [ h3 [class "header"] [text "Crossbars — Acrostic Player"]
          ]
        ]
