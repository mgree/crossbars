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
  | Clue Int String

type alias Model = 
    { title : String
    , author : String
    , quote : String
    , clues : List String
    }

initialModel =
    { title = ""
    , author = ""
    , quote = ""
    , clues = []
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
        Title title -> (updateClues { model | title = String.toUpper title }, Cmd.none)
        Author author -> (updateClues { model | author = String.toUpper author }, Cmd.none)
        Quote quote -> ({ model | quote = String.toUpper quote }, Cmd.none)
        Clue idx clue -> ({ model | clues = updateIndex idx clue model.clues}, Cmd.none)

updateClues : Model -> Model
updateClues model =
    let initials = initialism model 

        len = String.length initials

        shortenedClues = List.take len model.clues

        numMissing = len - List.length shortenedClues

        lengthenedClues = shortenedClues ++ List.repeat numMissing ""
                   
    in
    
        { model | clues = lengthenedClues }

view : Model -> Html Msg
view model = 
    let initials = initialism model in

    div [] 
        [ h1 [] [text "Crossbars Acrostic Constructor"]
        , section [id "quote"]
            [ textInput "Title" model.title Title
            , textInput "Author" model.author Author
            , textarea [ placeholder "Quote", onInput Quote, rows 5, cols 50 ] [text model.quote]
            ]
        , let 
              initialismHist = letterHist initials

              quoteHist = letterHist model.quote 

              missingHist = histDifference quoteHist initialismHist

              viable = isEmptyHist missingHist

              clueHist = letterHist (String.concat model.clues)

              remainingHist = histDifference clueHist quoteHist

          in

            section [id "stats"]
                [ div [] [text "Words: ", model |> numWords |> String.fromInt |> text]
                , div [] [text "Letters: ", model |> numLetters |> String.fromInt |> text]
                ,
                    if viable
                    then text "Viable acrostic"
                    else div [] [ text "Non-viable acrostic; initialism needs: "
                                , histToShortString missingHist |> text
                                ]
                , histToHtml remainingHist
                ]
        ,

            section [id "clues"]
            [ ol [type_ "A"] 
                 (List.map3 viewClue
                      (List.range 0 (String.length initials - 1))
                      (String.toList initials) 
                      model.clues)
            ]
        ]

viewClue : Int -> Char -> String -> Html Msg
viewClue index initial clue = 
    li []
        [textInput ("Clue starting with " ++ String.fromChar initial) clue (Clue index)]

{- Acrostic functions -}

type alias Hist = Dict Char Int

histogramChars : String -> List Char
histogramChars s =
    s |> String.toUpper 
      |> String.toList
      |> List.filter Char.isAlphaNum

initialism : Model -> String
initialism model = model.author ++ model.title |> String.filter Char.isAlphaNum

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
  
updateIndex : Int -> a -> List a -> List a
updateIndex n v l =
    case l of
        [] -> [v]
        h::t -> if n == 0
                then v::t
                else h::updateIndex (n-1) v t
