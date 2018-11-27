
module Language.BackEnd.Evaluator.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model
import Language.FrontEnd.AST
import Language.BackEnd.Evaluator.Types
import Language.BackEnd.Evaluator.Nodes.Identifier
import Data.Scientific

-- ─── TYPES ──────────────────────────────────────────────────────────────────────

type MasterEvalResult = Either String ( [Scientific], Model )

-- ─── MASTER EVAL ────────────────────────────────────────────────────────────────

masterEval :: AST -> Model -> MasterEvalResult
masterEval ast model =
    case eval ast $ prototype model of
        Left error -> Left error
        Right x -> Right ( [ x ], model )

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

eval :: EvalSignature
eval astNode scopePrototype =
    case astNode of
        ASTIdentifier _ ->
            evalIdentifier astNode scopePrototype
        ASTNumber x ->
            Right x
        _ ->
            Left $ "Undefined AST Node " ++ show astNode

-- ────────────────────────────────────────────────────────────────────────────────
