module SMT exposing (..)

smtAssert : String -> String
smtAssert prop = "(assert " ++ prop ++ ")"

smtWordFun : String
smtWordFun = "word-of"

smtWordOf : String -> String
smtWordOf x = "(" ++ smtWordFun ++ " " ++ x ++")"
             
smtEq : String -> String -> String
smtEq l r = "(= " ++ l ++ " " ++ r ++ ")"
                 
smtOr : List String -> String
smtOr props =
    case props of
        [] -> "true"
        [prop] -> prop
        _ -> "(or " ++ String.join " " props ++ ")"

smtAnd : List String -> String
smtAnd props =
    case props of
        [] -> "true" -- weird, I know, but probably the right default for our case. shouldn't come up.
        [prop] -> prop
        _ -> "(and " ++ String.join " " props ++ ")"

smtNot : String -> String
smtNot prop = "(not " ++ prop ++ ")"

smtAscending : List String -> String
smtAscending vars =
    case vars of
        [] -> "true"
        [var] -> "true"
        _ -> "(< " ++ String.join " " vars ++ ")"

smtDistinct : List String -> String
smtDistinct vars =
    case vars of
        [] -> "true"
        [var] -> "true"
        _ -> "(distinct " ++ String.join " " vars ++ ")"

