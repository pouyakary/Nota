
module Language.BackEnd.Evaluator.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model
import Language.FrontEnd.AST
import Language.BackEnd.Evaluator.Types
import Language.BackEnd.Evaluator.Nodes.Identifier
import Language.BackEnd.Evaluator.Nodes.BinaryOperator
import Language.BackEnd.Evaluator.Nodes.Negation
import Language.BackEnd.Evaluator.Nodes.FunctionCall

-- ─── TYPES ──────────────────────────────────────────────────────────────────────

type MasterEvalResult = Either String ( [ Double ], Model )

-- ─── MASTER EVAL ────────────────────────────────────────────────────────────────

masterEval :: AST -> Model -> String -> MasterEvalResult
masterEval ast model inputString =
    case eval ast $ prototype model of
        Left error -> Left error
        Right x -> Right ( [ x ], newModel )
    where
        newModel =
            model { history = ( history model ) ++ [ inputString ] }

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

eval :: LeafEvalSignature
eval astNode scopePrototype =
    case astNode of
        ASTBinaryOperator op left right ->
            evalBinaryOperator eval ( ASTBinaryOperator op left right ) scopePrototype
        ASTIdentifier _ ->
            evalIdentifier astNode scopePrototype
        ASTParenthesis x ->
            eval x scopePrototype
        ASTNegation _ ->
            evalNegation eval astNode scopePrototype
        ASTNumber x ->
            Right x
        ASTFunctionCall name args ->
            evalFunctionCall eval (ASTFunctionCall name args) scopePrototype
        _ ->
            Left $ "Undefined AST Node " ++ show astNode

-- ────────────────────────────────────────────────────────────────────────────────
