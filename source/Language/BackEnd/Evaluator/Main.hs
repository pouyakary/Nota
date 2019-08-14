
module Language.BackEnd.Evaluator.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.BackEnd.Evaluator.Nodes.Assignment
import Language.BackEnd.Evaluator.Nodes.BinaryOperator
import Language.BackEnd.Evaluator.Nodes.FunctionCall
import Language.BackEnd.Evaluator.Nodes.Identifier
import Language.BackEnd.Evaluator.Nodes.Negation
import Language.BackEnd.Evaluator.Nodes.Versus
import Language.BackEnd.Evaluator.Types
import Language.FrontEnd.AST
import Model

-- ─── MASTER EVAL ────────────────────────────────────────────────────────────────

masterEval :: AST -> Model -> String -> MasterEvalResult
masterEval ast model inputString =
    case ast of
        ASTVersus [ ] ->
            appendHistoryToModel ( MasterEvalResultRight [0] model ) model inputString
        ASTVersus parts ->
            case evalVersus eval parts model of
                Left error ->
                    Left error
                Right result ->
                    appendHistoryToModel result model inputString

        ASTAssignment name value ->
            case evalAssignment eval ast model of
                Left error ->
                    Left error
                Right result ->
                    appendHistoryToModel result model inputString
        _ ->
            case eval ast ( prototype model ) of
                Left error ->
                    Left error
                Right result ->
                    appendHistoryToModel ( MasterEvalResultRight [result] model ) model inputString

    where
        appendHistoryToModel ( MasterEvalResultRight resultValue resultModel ) model inputString =
            Right $ MasterEvalResultRight resultValue modelWithHistory where
                modelWithHistory =
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
