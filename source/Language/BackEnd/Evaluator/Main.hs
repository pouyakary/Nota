
module Language.Evaluator.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model
import Language.FrontEnd.AST
import Language.Evaluator.Types

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

eval :: EvalSignature
eval astNode scopePrototype functionsParams =
    case astNode of
        ASTNumber x -> Right x
        _ -> Left "Undefined AST Node " ++ $ show astNode

-- ────────────────────────────────────────────────────────────────────────────────
