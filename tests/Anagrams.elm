module Anagrams exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Hist exposing (Hist)
import Wordlist exposing (..)

import Util exposing (..)

import Parser

suite : Test
suite =
    describe "wordlist"
        [ test "AC" <|
              \_ -> 
                "AC" |>
                String.toList |>
                anagramsFor testingWordlist sampleHist |>
                Expect.equal ["ACF"]
        , test "lookup on word hits behaves correctly" <|
            \_ ->
                sampleWords |>
                List.all
                    (\w -> (trieLookup w sampleWL |> Maybe.map .word) == Just w) |>
                Expect.true "Expected all words to be present."                
        , fuzzWith { runs = 1000 } string "lookup behaves the same as a naive implementation" <|
            \s ->
                naiveWordlistLookup s sampleNWL |>
                Expect.equal (trieLookup s sampleWL)
        , fuzzWith { runs = 1000 } string "suffixes behaves the same as a naive implementation" <|
            \s ->
                naiveWordlistSuffixes (String.toList s) s sampleNWL |>
                Expect.equal (trieSuffixes (String.toList s) s sampleWL)
        , test "parse list of words w/final newline" <|
            \_ ->
                testingWords |>
                String.join "\n" |>
                (\s -> s ++ "\n") |>
                Wordlist.load "testing" |>
                Expect.equal testingWordlist
        , test "parse list of words w/o final newline" <|
            \_ ->
                testingWords |>
                String.join "\n" |>
                Wordlist.load "testing" |>
                Expect.equal testingWordlist
        ]

testingWords : List String
testingWords = ["ABC", "ABCD", "ABF", "ABD", "ACF", "AFC", "BCD", "BFD", "BDF", "BFF"]

testingWordlist : Wordlist
testingWordlist = 
    generateWordlist "testing" testingWords

sampleNWL : NaiveWordlist
sampleNWL = generateNaiveWordlist "testing" sampleWords

sampleWL : Wordlist
sampleWL = generateWordlist "testing" sampleWords

sampleWords : List String
sampleWords = 
    ["ABC", "ABCD", "ABF", "ABD", "ACF", "AFC",
     "BCD", "BFD", "BDF", "BFF"]

sampleHist : Hist
sampleHist = Hist.fromString "ABCDFF"

{- model for comparison -}
type alias NaiveWordlist = List Entry

emptyNaiveWordlist : NaiveWordlist
emptyNaiveWordlist = []

naiveWordlistInsert : Entry -> NaiveWordlist -> NaiveWordlist
naiveWordlistInsert e nwl = 
    if String.length e.word >= 3 
    then insertWith compareEntry e nwl
    else nwl

naiveWordlistLookup : String -> NaiveWordlist -> Maybe Entry
naiveWordlistLookup word nwl =
    case word |> String.toList of
        c1::c2::c3::_ -> 
            nwl |>
            List.filter (\entry -> entry.word == word) |>
            List.head
        _ -> Nothing

naiveWordlistSuffixes : List Char -> String -> NaiveWordlist -> List Entry
naiveWordlistSuffixes word s nwl =
    if List.length word <= 1
    then []
    else List.filter (.word >> String.startsWith s) nwl

generateNaiveWordlist : String -> List String -> NaiveWordlist
generateNaiveWordlist source words =
    words |>
    List.sort |>
    List.map 
        (\word ->
             { word = word
             , source = source
             , desc = ""
             , url = ""
             })
