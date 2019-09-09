module Encoding exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)

import Json.Decode as Decode

import Time

import Puzzle exposing (Puzzle)

suite : Test
suite =
    Test.concat
        [ describe "Puzzle JSON encoder"
            [ test "empty puzzle round trips" <|
                  \_ ->
                      roundTrip Puzzle.empty
            , fuzz fuzzPuzzle "round trips" roundTrip
            , test "empty puzzle blank round trips" <|
                \_ ->
                    blankRoundTrip (Puzzle.toBlank Puzzle.empty)
            , fuzz fuzzPuzzle "blank round trips" (Puzzle.toBlank >> blankRoundTrip)
            ]
        ]

roundTrip : Puzzle -> Expectation
roundTrip puzzle =
    puzzle |>
    Puzzle.encode |>
    Decode.decodeValue Puzzle.decoder |>
    Expect.equal (Ok puzzle)

blankRoundTrip : Puzzle.Blank -> Expectation
blankRoundTrip puzzle =
    puzzle |>
    Puzzle.encodeBlank |>
    Decode.decodeValue Puzzle.blankDecoder |>
    Expect.equal (Ok puzzle)

fuzzPuzzle : Fuzzer Puzzle
fuzzPuzzle =
    Fuzz.map5
        (\title author quote clues phase timeModified ->
             { title = title
             , author = author
             , quote = quote
             , clues = clues
             , phase = phase
             , timeModified = timeModified
             })
        Fuzz.string
        Fuzz.string
        Fuzz.string
        (Fuzz.list fuzzClue)
        fuzzPhase |> 
    Fuzz.andMap fuzzPosixTime

fuzzClue : Fuzzer Puzzle.Clue
fuzzClue = 
    Fuzz.map3
        (\hint text answer ->
             { hint = hint
             , text = text
             , answer = answer
             }) 
        Fuzz.string 
        Fuzz.string
        (Fuzz.list (Fuzz.tuple (Fuzz.maybe Fuzz.int, Fuzz.char)))

fuzzPhase : Fuzzer Puzzle.Phase
fuzzPhase = 
    Puzzle.phases |>
    List.map Fuzz.constant |>
    Fuzz.oneOf 

fuzzPosixTime : Fuzzer Time.Posix
fuzzPosixTime = Fuzz.int |> Fuzz.map Time.millisToPosix

                     
