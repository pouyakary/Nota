
module Language.Evaluator.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model
import Language.FrontEnd.AST
import Language.BackEnd.Evaluator.Types
import Language.BackEnd.Evaluator.Nodes.Identifier

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

eval :: EvalSignature
eval astNode scopePrototype =
    case astNode of
        ASTIdentifier _ ->
            evalIdentifier astNode
        ASTNumber x ->
            Right x
        _ ->
            reportOnUndefinedNode
        where
            reportOnUndefinedNode =
                Left "Undefined AST Node " ++ $ show astNode

-- ────────────────────────────────────────────────────────────────────────────────
