module Puzzle exposing 
    ( Puzzle
    , empty
    , compare
    , equal
    , encode
    , decoder

    , Phase(..)
    , phases
    , stringOfPhase

    , Clue
    , defaultClue
    , letterFor

    , setPhase
    , setTimeModified
    , setTitle
    , setAuthor
    , setQuote
    , clearNumbering
    , updateNumbering
    , updateHint
    , updateAnswer
    , fixupAnswerInitials
        
    , clueAnswer
    , clueFor
    , description
    , shortDescription
    , initialism
    , quoteIndex
    , quoteIndices
    , quoteIndexWords
    , quoteIndexUses

    , duplicateNumberings
    , unclued
    , unnumbered

    , Blank
    , BlankClue
    , completedBlank
    , sameBlank
    , encodeBlank
    , blankDecoder
    , toBlank
    , asQuoteIn

    , Problem
    , problemToString
    , isValidBlank

    , exportJSON
    )

import Dict exposing (Dict)

import Json.Encode as Encode
import Json.Decode as Decode

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

empty :  Puzzle
empty =
    { title = ""
    , author = ""
    , quote = ""
    , clues = []
    , phase = QuoteEntry
    , timeModified = Time.millisToPosix 0
    }

shortDescription : Puzzle -> String
shortDescription puzzle = 
    String.toUpper puzzle.author ++ " — " ++ String.toUpper puzzle.title

description : Time.Zone -> Puzzle -> String
description here puzzle =
    shortDescription puzzle ++ " (" ++ iso8601DateTime here puzzle.timeModified ++ ")"
    
compare : Puzzle -> Puzzle -> Order
compare puz1 puz2 =
    Basics.compare 
        (Time.posixToMillis puz1.timeModified) 
        (Time.posixToMillis puz2.timeModified)

equal : Puzzle -> Puzzle -> Bool
equal puz1 puz2 = compare puz1 puz2 == EQ

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

decoder : Decode.Decoder Puzzle
decoder =
    Decode.map6
        (\title author quote clues phase timeModified ->
             { title = title
             , author = author
             , quote = quote
             , clues = clues
             , phase = phase
             , timeModified = timeModified
             })
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "quote" Decode.string)
        (Decode.field "clues" (Decode.list clueDecoder))
        (Decode.field "phase" phaseDecoder)
        (Decode.field "timeModified" (Decode.int |>
                                               Decode.map Time.millisToPosix))

clueDecoder : Decode.Decoder Clue
clueDecoder =
    Decode.map3
        (\hint text answer ->
             { hint = hint
             , text = text
             , answer = answer
             })
        (Decode.field "hint" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "answer" (Decode.list answerDecoder))

answerDecoder : Decode.Decoder (Maybe Int, Char)
answerDecoder =
    Decode.map2
        Tuple.pair
        (Decode.field "number" (Decode.nullable Decode.int))
        (Decode.field "char" decodeChar)

phaseDecoder : Decode.Decoder Phase
phaseDecoder =
    Decode.string |> Decode.andThen
        (\s ->
             case s of
                 "QuoteEntry" -> Decode.succeed QuoteEntry
                 "Anagramming" -> Decode.succeed Anagramming
                 "CluingLettering" -> Decode.succeed CluingLettering
                 _ -> Decode.fail ("invalid phase '" ++ s ++ "'"))

encode : Puzzle -> Encode.Value
encode puzzle =
    Encode.object
        [ ("title", Encode.string puzzle.title)
        , ("author", Encode.string puzzle.author)
        , ("quote", Encode.string puzzle.quote)
        , ("clues", Encode.list encodeClue puzzle.clues)
        , ("phase", encodePhase puzzle.phase)
        , ("timeModified", Encode.int <| Time.posixToMillis <| puzzle.timeModified)
        ]

encodeClue : Clue -> Encode.Value
encodeClue clue =
    Encode.object
        [ ("hint", Encode.string clue.hint)
        , ("text", Encode.string clue.text)
        , ("answer", Encode.list encodeAnswer clue.answer)
        ]

encodeAnswer : (Maybe Int, Char) -> Encode.Value
encodeAnswer (mNum, c) =
    Encode.object
        [ ("number", encodeNullable Encode.int mNum)
        , ("char", Encode.string <| String.fromChar <| c)
        ]

encodePhase : Phase -> Encode.Value
encodePhase phase =
    Encode.string <| case phase of
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

emptyBlank : Blank
emptyBlank = toBlank empty

sameBlank : Blank -> Blank -> Bool
sameBlank b1 b2 =
    (b1 |> Debug.log "b1").quoteWordLengths == (b2 |> Debug.log "b2").quoteWordLengths &&
    List.all identity (List.map2 (==) b1.clues b2.clues)

completedBlank : Blank -> Bool
completedBlank b =
    List.all (\mc -> mc /= Nothing) b.quote

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

encodeBlank : Blank -> Encode.Value
encodeBlank b =
    Encode.object
        [ ("quote", if List.isEmpty b.quote || List.all (\qc -> qc == Nothing) b.quote               
                    then Encode.null
                    else Encode.list (Maybe.map String.fromChar >> encodeNullable Encode.string) b.quote)
        , ("quoteWordLengths", Encode.list Encode.int b.quoteWordLengths)
        , ("boardColumns", Encode.int b.boardColumns)
        , ("clues", Encode.list encodeBlankClue b.clues)
        ]

encodeBlankClue : BlankClue -> Encode.Value
encodeBlankClue c =
    Encode.object
        [ ("hint", Encode.string c.hint)
        , ("answer", Encode.list Encode.int c.answer)
        ]

blankDecoder : Decode.Decoder Blank
blankDecoder =
    Decode.map4
        (\mQuote quoteWordLengths boardColumns clues ->
             { quote = case mQuote of
                           Nothing -> List.repeat (List.sum quoteWordLengths) Nothing
                           Just quote -> quote
             , quoteWordLengths = quoteWordLengths
             , boardColumns = boardColumns
             , clues = clues
             })
        (Decode.field "quote" (Decode.nullable (Decode.list (Decode.nullable decodeChar))))
        (Decode.field "quoteWordLengths" (Decode.list Decode.int))
        (Decode.field "boardColumns" Decode.int)
        (Decode.field "clues" (Decode.list blankClueDecoder))

blankClueDecoder : Decode.Decoder BlankClue
blankClueDecoder =
    Decode.map2
        (\hint answer -> { hint = hint, answer = answer })
        (Decode.field "hint" Decode.string)
        (Decode.field "answer" (Decode.list Decode.int))

-- VALIDATION

type Problem = QuoteLengthMismatch Int Int
             | MissingClueNumbers (List Int)
             | DuplicateClueNumbers (List (Int, (List Int)))

problemToString : Problem -> String
problemToString p =
    case p of
        QuoteLengthMismatch q qwl ->
            "quote is of length " ++ String.fromInt q ++ " but quote word divisions indicate a quote of length " ++ String.fromInt qwl
        MissingClueNumbers nums -> 
            "missing clue numbers (" ++ String.join ", " (List.map String.fromInt nums) ++ ")"
        DuplicateClueNumbers nums ->
            "duplicate clue numbers (" ++ 
                String.join "; " 
                    (nums |>
                     List.map 
                         (\(qIndex, clueIndices) ->
                              String.fromInt qIndex ++ " in " ++
                              String.join "," (List.map letterFor clueIndices))) ++ 
             ")"         

isValidBlank : Blank -> List Problem
isValidBlank puzzle =
    let
        quoteLength = List.length puzzle.quote 

        qwlLength = List.sum puzzle.quoteWordLengths

        correctNumbers = List.range 0 (List.length puzzle.quote - 1)

        actualNumbers =
            puzzle.clues |>
            List.concatMap .answer |>
            List.sort

        missingNumbers = 
            correctNumbers |>
            List.filter
                (\num ->
                     not (List.member num actualNumbers))

        numberCounts =
            puzzle.clues |>
            List.indexedMap (\index clue -> (index, clue.answer)) |>
            List.concatMap (\(index,answer) -> List.map (Tuple.pair index) answer) |>
            List.foldr
                (\(index, qIndex) d -> updateCons qIndex index d)
                Dict.empty

        dupIndices =
            numberCounts |>
            Dict.filter (\_ idxs -> List.length idxs /= 1) |>
            Dict.toList
            
    in
        (if quoteLength == qwlLength 
         then []
         else [QuoteLengthMismatch quoteLength qwlLength]) ++ 
        (if List.isEmpty missingNumbers
         then []
         else [MissingClueNumbers missingNumbers]) ++
        (if List.isEmpty dupIndices
         then []
         else [DuplicateClueNumbers dupIndices])
            

isValid : Puzzle -> Bool
isValid puzzle =
    let 
        numCluesValid = List.length puzzle.clues == 
                          (String.length puzzle.title + String.length puzzle.author)

        initialsValid = 
            (initialism puzzle |>
             String.toList |>
             List.map Just) 
            ==
            (puzzle.clues |>
             List.map .answer |>
             List.map List.head |>
             List.map (Maybe.map Tuple.second))

    in
    
        numCluesValid &&
        initialsValid

exportJSON : Puzzle -> String
exportJSON puzzle =
    puzzle |>
    toBlank |>
    encodeBlank |>
    Encode.encode 0
