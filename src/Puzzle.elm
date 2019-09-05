module Puzzle exposing (..)

import Dict exposing (Dict)

import Json.Encode
import Json.Decode

import Time

import Util exposing (..)

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

unnumbered : Puzzle -> List (Int, Int)
unnumbered puz =
    puz.clues |>
    List.indexedMap
        (\cIndex clue ->
             clue.answer |>
             List.indexedMap
                 (\ansIndex (mNum,_) ->
                      case mNum of
                          Nothing -> Just (cIndex, ansIndex)
                          Just _ -> Nothing) |>
             List.filterMap identity) |>
    List.concat

unclued : Puzzle -> List Int
unclued puz =
    puz.clues |>
    List.indexedMap
        (\cIndex clue ->
             if String.isEmpty clue.hint
             then Just cIndex
             else Nothing) |>
    List.filterMap identity

duplicateNumberings : Puzzle -> List (Int, List (Int, Int))
duplicateNumberings puz =
    puz.clues |>
    List.indexedMap 
        (\cIndex clue -> 
             clue.answer |>
             List.indexedMap
                 (\ansIndex (mNum,_) ->
                      case mNum of
                          Nothing -> []
                          Just num -> [(num, (cIndex, ansIndex))]) |>
             List.concat) |>
    List.concat |>
    List.foldr
        (\(qIndex, (cIndex, ansIndex)) d ->
             updateCons qIndex (cIndex, ansIndex) d)
        Dict.empty |>
    Dict.filter (\qIndex l -> List.length l > 1) |>
    Dict.toList

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

-- ACROSTIC FUNCTIONS

stringOfPhase : Phase -> String
stringOfPhase p =
    case p of
        QuoteEntry -> "Quote entry"
        Anagramming -> "Anagramming"
        CluingLettering -> "Cluing and lettering"

phases : List Phase
phases = [QuoteEntry, Anagramming, CluingLettering]
         
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

initialism : Puzzle -> String
initialism puzzle = puzzle.author ++ puzzle.title |> String.filter Char.isAlphaNum

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

-- SAVING/LOADING

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

-- BLANKS FOR PLAYING

type alias BlankClue =
    { hint : String
    , answer : List Int
    }

type alias Blank =
    { quote : List (Maybe Char)
    , quoteWordLengths : List Int
    , boardColumns : Int
    , clues : List BlankClue
    }

toBlank : Puzzle -> Blank
toBlank puzzle =
    { quote = puzzle.quote |>
              cleanChars |>
              List.map (always Nothing)
    , quoteWordLengths = puzzle.quote |>
                         String.words |>
                         List.map cleanChars |>
                         List.filter (not << List.isEmpty) |>
                         List.map List.length
    , boardColumns = 35
    , clues = puzzle.clues |>
              List.map toBlankClue
    }

toBlankClue : Clue -> BlankClue
toBlankClue clue =
    { hint = clue.hint
    , answer = clue.answer |>
               List.map (Tuple.first >> Maybe.withDefault (-1))
    }

asQuoteIn : Blank -> List (Maybe Char) -> Blank
asQuoteIn puzzle quote = { puzzle | quote = quote }
