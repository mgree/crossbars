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
        Title title -> ({ model | title = title }, Cmd.none)
        Author author -> ({ model | author = author }, Cmd.none)
        Quote quote -> ({ model | quote = quote }, Cmd.none)

view : Model -> Html Msg
view model = 
    div [] 
        [ h1 [] [text "Crossbars Acrostic Constructor"]
        , div []
            [ textInput "Title" model.title Title
            , textInput "Author" model.author Author
            , textarea [ placeholder "Quote", onInput Quote, rows 5, cols 50 ] [text model.quote]
            ]
        ]
