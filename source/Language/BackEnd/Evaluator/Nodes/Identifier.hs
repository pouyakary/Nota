
module Language.BackEnd.Evaluator.Nodes.Identifier ( evalIdentifier ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.Map (Map)
import qualified Data.Map as Map
import           Language.BackEnd.Evaluator.Types
import           Language.FrontEnd.AST
import           Model

-- ─── EVAL IDENTIFIER ────────────────────────────────────────────────────────────

evalIdentifier :: EvalSignature
evalIdentifier ( ASTIdentifier name ) scopePrototype =
    case Map.lookup name scopePrototype of
        Just x  -> Right x
        Nothing -> Left $ "Variable \"" ++ name ++ "\" does not exists."

-- ────────────────────────────────────────────────────────────────────────────────
