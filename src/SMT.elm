module SMT exposing 
    ( SolverState(..)
    , solverStateDecoder
    , assert
    , wordFun
    , wordOf
    , eq
    , or
    , and
    , not
    , ascending
    , distinct
    )

import Json.Decode as Decode

type SolverState = SolverUnloaded
                 | SolverDownloading
                 | SolverInitializing
                 | SolverReady
                 | SolverRunning

solverStateDecoder : Decode.Decoder SolverState
solverStateDecoder =
    Decode.string |>
    Decode.andThen
        (\s ->
             case s of
                 "SolverUnloaded" -> Decode.succeed SolverUnloaded
                 "SolverDownloading" -> Decode.succeed SolverDownloading
                 "SolverInitializing" -> Decode.succeed SolverInitializing
                 "SolverReady" -> Decode.succeed SolverReady
                 "SolverRunning" -> Decode.succeed SolverRunning
                 _ -> Decode.fail ("expected solver state, found '" ++ s ++ "'"))

assert : String -> String
assert prop = "(assert " ++ prop ++ ")"

wordFun : String
wordFun = "word-of"

wordOf : String -> String
wordOf x = "(" ++ wordFun ++ " " ++ x ++")"
             
eq : String -> String -> String
eq l r = "(= " ++ l ++ " " ++ r ++ ")"
                 
or : List String -> String
or props =
    case props of
        [] -> "true"
        [prop] -> prop
        _ -> "(or " ++ String.join " " props ++ ")"

and : List String -> String
and props =
    case props of
        [] -> "true" -- weird, I know, but probably the right default for our case. shouldn't come up.
        [prop] -> prop
        _ -> "(and " ++ String.join " " props ++ ")"

not : String -> String
not prop = "(not " ++ prop ++ ")"

ascending : List String -> String
ascending vars =
    case vars of
        [] -> "true"
        [var] -> "true"
        _ -> "(< " ++ String.join " " vars ++ ")"

distinct : List String -> String
distinct vars =
    case vars of
        [] -> "true"
        [var] -> "true"
        _ -> "(distinct " ++ String.join " " vars ++ ")"

