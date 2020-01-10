
module Language.BackEnd.Evaluator.Nodes.Versus ( evalVersus ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Time
import           Language.BackEnd.Evaluator.Types
import           Language.FrontEnd.AST
import           Model
import           System.IO.Unsafe

-- ─── EVAL VERSUS ────────────────────────────────────────────────────────────────

evalVersus :: EvalFunction -> [AST] -> Model -> MasterEvalResult
evalVersus ( evalFunc ) parts model =
    case evalArrayOfASTExpressions evalFunc parts model of
        Left error ->
            Left error
        Right result ->
            Right $ MasterEvalResultRight result model

-- ─── EVALUATE ARRAY OF AST EXPRESSIONS ──────────────────────────────────────────

evalArrayOfASTExpressions :: EvalFunction -> [AST] -> Model -> Either String [Double]
evalArrayOfASTExpressions ( evalFunc ) nodes model =
    case length nodes of
        0 ->
            Right [ ]
        _ ->
            case evalFunc (head nodes) model of
                Left error ->
                    Left error
                Right headResult ->
                    case evalArrayOfASTExpressions evalFunc (tail nodes) model of
                        Left error ->
                            Left error
                        Right tailResult ->
                            Right $ headResult : tailResult


-- ────────────────────────────────────────────────────────────────────────────────
