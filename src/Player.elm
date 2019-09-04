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
type alias Model = 
    { cursor : Cursor
    }

defaultModel : Model
defaultModel = 
    { cursor = Clues { clueIndex = 0
                     , letterIndex = 0
                     }
    }

withCursor : Cursor -> Model -> Model
withCursor cursor model = { model | cursor = cursor }

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
    case msg of
        UpdateIndex index mc -> (model, Cmd.none)

        SelectIndex cursor -> 
            ( model |>
              withCursor cursor
            , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = div [] []
