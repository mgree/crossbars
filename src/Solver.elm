module Solver exposing 
    ( generateNumberingProblem
    , applySMTNumbering
    , decodeSMTResult
    , SMTResult
    , missingResult
    , SMTAnswer(..)
    , SMTNumbering
    )

import Dict exposing (Dict)

import Json.Encode
import Json.Decode

import Parser exposing (Parser, (|.), (|=), symbol, end, succeed, spaces)

import Puzzle exposing (..)
import SMT
import Util exposing (..)

-- NUMBERING VIA SMT

type alias ConstraintVar = String

type alias ConstraintProblem = (List ConstraintVar, List Constraint)
    
type Constraint = IsInt ConstraintVar
                | OneOf ConstraintVar (List Int)
                | Distinct (List ConstraintVar)
                | NotAscending (List ConstraintVar)
                | NotSameWord (List ConstraintVar)

isDefn : Constraint -> Bool
isDefn c =
    case c of
        IsInt _ -> True
        _ -> False

generateNumberingProblem :  Puzzle -> Json.Encode.Value
generateNumberingProblem puzzle =
    puzzle |> 
    constraintsOfPuzzle (Puzzle.quoteIndices puzzle) |> 
    smt2OfConstraints (Puzzle.quoteIndexWords puzzle) |>
    Json.Encode.string

                  
constraintsOfPuzzle : Dict Char (List Int) -> Puzzle -> List Constraint
constraintsOfPuzzle qIndices puzzle =
    let
        varName clueIndex numIndex =
            "clue" ++ String.fromInt clueIndex ++ "_" ++
            "letter" ++ String.fromInt numIndex

        clueVarsByClue =
            puzzle.clues |>
            List.indexedMap
                (\clueIndex clue ->
                     clue.answer |> {- FIXME way to consider existing numbers? -}
                     List.indexedMap Tuple.pair |>
                     List.map (\(numIndex, (_, c)) -> (varName clueIndex numIndex, c)))
                
        clueVars = List.concat clueVarsByClue
                
        charConstraints =
            clueVars |> List.concatMap
                (\(v, c) ->
                     let uses = Dict.get (Char.toUpper c) qIndices |>
                                Maybe.withDefault [] {- yikes -}
                     in
                         [IsInt v, OneOf v uses])

        charUses = List.foldr (\(v,c) d -> updateCons c v d) Dict.empty clueVars

        disjointnessConstraints = charUses |>
                                  Dict.values |>
                                  List.map Distinct

        numberingConstraints =
            clueVarsByClue |>
            List.concatMap
                (\vs ->
                    let vars = List.map Tuple.first vs in 
                     [ NotAscending vars
                     , NotAscending (List.reverse vars)
                     , NotSameWord vars
                     ])
            
    in
    
    charConstraints ++ disjointnessConstraints ++ numberingConstraints

type alias SMTNumbering = List SMTNumberEntry
                
type alias SMTNumberEntry =
    { clue : Int
    , letter : Int
    , number : Int
    }

type alias SMTResult =
    { answer : SMTAnswer
    , elapsed : Int {- millis -}
    }

type SMTAnswer = SMTOk SMTNumbering
               | SMTTimeout
               | SMTFailed

missingResult : SMTResult
missingResult = { answer = SMTFailed
                   , elapsed = 0
                   }

applySMTNumbering : SMTNumbering -> Puzzle -> Puzzle
applySMTNumbering nums puz =
    let apply num newPuz =
            updateNumbering num.clue num.letter (Just num.number) newPuz
    in
        List.foldr apply puz nums

decodeSMTResult : Json.Decode.Decoder SMTResult
decodeSMTResult = 
    Json.Decode.map2 
        (\elapsed stdout ->
             let answer = String.join "\n" stdout |>
                          Parser.run smtAnswerParser |>
                          Result.withDefault SMTFailed
             in { answer = answer
                , elapsed = elapsed
                })
        (Json.Decode.field "elapsed" Json.Decode.int)
        (Json.Decode.field "stdout" (Json.Decode.list Json.Decode.string))

smtAnswerParser : Parser SMTAnswer
smtAnswerParser =
  Parser.oneOf
      [ succeed SMTFailed
        |. symbol "unsat"
      , succeed SMTTimeout
        |. symbol "unknown"
      , succeed SMTOk
        |. symbol "sat"
        |. spaces
        |= smtModelParser
      ]
        
smtModelParser : Parser SMTNumbering
smtModelParser =
    succeed identity
        |. spaces
        |. symbol "("
        |. spaces
        |= listOf smtValueParser
        |. spaces
        |. symbol ")"
        |. spaces
        |. end

smtValueParser : Parser SMTNumberEntry
smtValueParser =
    succeed (\clue letter number ->
                 { clue = clue
                 , letter = letter
                 , number = number
                 })
        |. spaces
        |. symbol "("
        |. symbol "clue"
        |= Parser.int
        |. symbol "_letter"
        |= Parser.int
        |. spaces
        |= Parser.int
        |. symbol ")"

smt2OfConstraint : Constraint -> String
smt2OfConstraint c =
    case c of
        IsInt var -> ("(declare-const " ++ var ++ " Int)")

        OneOf var ns ->
            ns |>
            List.map (\n -> SMT.eq var (String.fromInt n)) |>
            SMT.or |>
            SMT.assert

        Distinct [] -> SMT.assert "true"
        Distinct vars -> 
            vars |>
            SMT.distinct |>
            SMT.assert

        NotAscending vars ->
            vars |>
            SMT.ascending |>
            SMT.not |>
            SMT.assert
                 
        NotSameWord [] -> SMT.assert "true"
        NotSameWord [_] -> SMT.assert "true"             
        NotSameWord vars ->
            vars |>
            List.map SMT.wordOf |>
            SMT.distinct |>
            SMT.assert

smt2OfConstraints : Dict Int Int -> List Constraint -> String
smt2OfConstraints qIndexWords constraints =
    let

        (defnConstraints, assertConstraints) = List.partition isDefn constraints

        vars = defnConstraints |>
               List.filterMap
                   (\c ->
                        case c of
                            IsInt v -> Just v
                            _ -> Nothing)
                                               
        defns = defnConstraints |>
                List.map smt2OfConstraint

        wordFun =
            let
                {- METHOD 1: axiomatize (currently used, works much better) -}
                decl = "(declare-fun " ++ SMT.wordFun ++ " (Int) Int)"

                vals =
                    qIndexWords |>
                    Dict.foldr
                        (\x wordNum eqs ->
                             ("(= " ++ SMT.wordOf (String.fromInt x) ++ " " ++ (String.fromInt wordNum) ++ ")") :: eqs)
                        [] |>
                    SMT.and |>
                    SMT.assert

                {- METHOD 2: define as a function/macro -}
                conds =
                    qIndexWords |>
                    Dict.foldr
                        (\x wordNum otw ->
                             "(ite (= n " ++ String.fromInt x ++ ") " ++ (String.fromInt wordNum) ++ " " ++ otw ++ ")")
                        "-1"
                        
                defn = "(define-fun " ++ SMT.wordFun ++ " ((n Int)) Int " ++ conds ++ ")"
                        
            in
                [decl, vals] -- [defn]
                                          
        assertions = assertConstraints |>
                     List.map smt2OfConstraint
                        
        commands = [ {- "(set-option :timeout 2000)"
                   , -} "(set-option :produce-models true)"] ++
                   defns ++ wordFun ++ assertions ++
                   ["(check-sat)", "(get-value (" ++ String.join " " vars ++ "))"]
                           
    in
        String.join "\n" commands

