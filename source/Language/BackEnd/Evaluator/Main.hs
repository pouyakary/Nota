
module Language.BackEnd.Evaluator.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model
import Language.FrontEnd.AST
import Language.BackEnd.Evaluator.Types
import Language.BackEnd.Evaluator.Nodes.Identifier
import Language.BackEnd.Evaluator.Nodes.BinaryOperator

-- ─── TYPES ──────────────────────────────────────────────────────────────────────

type MasterEvalResult = Either String ( [ P50 ], Model )

-- ─── MASTER EVAL ────────────────────────────────────────────────────────────────

masterEval :: AST -> Model -> MasterEvalResult
masterEval ast model =
    case eval ast $ prototype model of
        Left error -> Left error
        Right x -> Right ( [ x ], model )

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
        ASTNumber x ->
            Right x
        _ ->
            Left $ "Undefined AST Node " ++ show astNode

-- ────────────────────────────────────────────────────────────────────────────────
