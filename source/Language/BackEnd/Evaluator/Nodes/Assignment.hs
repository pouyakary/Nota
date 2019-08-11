
module Language.BackEnd.Evaluator.Nodes.Assignment ( evalAssignment ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Time
import           Language.BackEnd.Evaluator.Types
import           Language.FrontEnd.AST
import           Model
import           System.IO.Unsafe

-- ─── EVAL ASSIGNMENT ────────────────────────────────────────────────────────────

evalAssignment :: LeafEvalSignature -> AST -> Model -> MasterEvalResult
evalAssignment ( evalFunc ) ( ASTAssignment ( ASTIdentifier name ) value ) model =
    case evalFunc value $ previousPrototype of
        Left error ->
            Left error
        Right result ->
            Right $ MasterEvalResultRight [ result ] newModel where
                newPrototype =
                    Map.insert name result previousPrototype
                newModel =
                    model { prototype = newPrototype }
    where
        previousPrototype = prototype model

-- ────────────────────────────────────────────────────────────────────────────────
