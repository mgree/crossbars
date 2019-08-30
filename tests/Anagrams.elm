module Anagrams exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Histogram exposing (Hist)
import Wordlist exposing (..)

sampleHist : Hist
sampleHist = Histogram.letterHist "ABCDFF"

suite : Test
suite =
    describe "wordlist"
        [ test "AC" <|
              \_ -> 
                "AC" |>
                String.toList |>
                anagramsFor testingWordlist sampleHist |>
                Expect.equal ["ACF"]
        ]
