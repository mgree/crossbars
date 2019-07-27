import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Browser


type alias Flags = ()

type Msg =
    Title String
  | Author String
  | Quote String

type alias Model = 
    { title : String
    , author : String
    , quote : String
    }

initialModel =
    { title = ""
    , author = ""
    , quote = ""
    }

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

init : Flags -> (Model, Cmd Msg)
init flags = (initialModel, Cmd.none)

textInput : String -> String -> (String -> msg) -> Html msg
textInput p v toMsg = input [ type_ "text", placeholder p, value v, onInput toMsg ] []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Title title -> ({ model | title = String.toUpper title }, Cmd.none)
        Author author -> ({ model | author = String.toUpper author }, Cmd.none)
        Quote quote -> ({ model | quote = String.toUpper quote }, Cmd.none)

view : Model -> Html Msg
view model = 
    div [] 
        [ h1 [] [text "Crossbars Acrostic Constructor"]
        , div []
            [ textInput "Title" model.title Title
            , textInput "Author" model.author Author
            , textarea [ placeholder "Quote", onInput Quote, rows 5, cols 50 ] [text model.quote]
            ]
        , let 
              initialismHist = letterHist (initialism model)

              quoteHist = letterHist model.quote 

              missingHist = histDifference quoteHist initialismHist

              viable = isEmptyHist missingHist

          in

            div [id "stats"]
                [ div [] [text "Words: ", model |> numWords |> String.fromInt |> text]
                , div [] [text "Letters: ", model |> numLetters |> String.fromInt |> text]
                ,
                    if viable
                    then text "Viable acrostic"
                    else div [] [ text "Non-viable acrostic; initialism needs: "
                                , histToShortString missingHist |> text
                                ]
                , histToHtml quoteHist
                ]
        ]

{- Acrostic functions -}

type alias Hist = Dict Char Int

histogramChars : String -> List Char
histogramChars s =
    s |> String.toUpper 
      |> String.toList
      |> List.filter Char.isAlphaNum

initialism : Model -> String
initialism model = model.author ++ model.title

numWords : Model -> Int
numWords model = 
    model.quote 
        |> String.words 
        |> List.filter (\w -> w |> String.isEmpty |> not) 
        |> List.length

numLetters : Model -> Int
numLetters model = 
    model.quote 
        |> String.words
        |> List.map String.length 
        |> List.sum

emptyHist : Hist
emptyHist = Dict.empty

emptyLetterHist : Hist
emptyLetterHist = 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
        |> String.toList
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
        emptyHist (histogramChars s)

cleanLetterHist : Hist -> Hist
cleanLetterHist h =
    Dict.union h emptyLetterHist 
        |> Dict.filter (\c cnt -> cnt >= 0)

histToShortString : Hist -> String
histToShortString h =
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.toList
      |> List.map (\(c, count) -> c |> String.fromChar |> String.repeat count)
      |> String.concat

histToHtml : Hist -> Html msg
histToHtml h =
    div []
        (h |> cleanLetterHist
           |> Dict.toList
           |> List.map (\(c,cnt) -> 
                            span [] [ span [] [text (String.fromChar c)]
                                    , span [] [text (String.fromInt cnt)]
                                    ])
        )

histDifference : Hist -> Hist -> Hist
histDifference hSub hSup = 
    Dict.merge
        (\c cSub      d -> Dict.insert c (-cSub) d)
        (\c cSub cSup d -> Dict.insert c (cSup - cSub) d)
        (\c      cSup d -> Dict.insert c cSup d)
        hSub hSup
        Dict.empty

isEmptyHist : Hist -> Bool
isEmptyHist h = 
    h |> Dict.filter (\c cnt -> cnt > 0)
      |> Dict.isEmpty
  
