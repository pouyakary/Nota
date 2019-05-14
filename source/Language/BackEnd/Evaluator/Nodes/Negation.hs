
module Language.BackEnd.Evaluator.Nodes.Negation ( evalNegation ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.List
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Language.BackEnd.Evaluator.Types
import           Language.FrontEnd.AST
import           Model

-- ─── EVAL NEGATION ──────────────────────────────────────────────────────────────

evalNegation :: StemEvalSignature
evalNegation ( evalFunc ) ( ASTNegation node ) scopePrototype =
    case evalFunc node scopePrototype of
        Right x -> Right $ -x
        Left  x -> Left x

-- ────────────────────────────────────────────────────────────────────────────────
