
module Language.BackEnd.Evaluator.Nodes.BinaryOperator ( evalBinaryOperator ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.List
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Language.BackEnd.Evaluator.Types
import           Language.FrontEnd.AST
import           Model

-- ─── EVAL BINARY OPERATORS ──────────────────────────────────────────────────────

evalBinaryOperator :: StemEvalSignature
evalBinaryOperator ( evalFunc ) ( ASTBinaryOperator op left right ) scopePrototype =
    case evalFunc left scopePrototype of
        Left  leftErr ->
            Left leftErr
        Right evaluatedLeft ->
            case evalFunc right scopePrototype of
                Left rightErr ->
                    Left rightErr
                Right evaluatedRight ->
                    Right result where
                        result =
                            case op of
                                Sum ->
                                    evaluatedLeft + evaluatedRight
                                Sub ->
                                    evaluatedLeft - evaluatedRight
                                Div ->
                                    evaluatedLeft / evaluatedRight
                                Mul ->
                                    evaluatedLeft * evaluatedRight
                                Pow ->
                                    evaluatedLeft ** evaluatedRight
                                Equ ->
                                    if evaluatedLeft == evaluatedRight then 1 else 0
                                NEq ->
                                    if evaluatedLeft /= evaluatedRight then 1 else 0
                                Mod ->
                                    evaluatedLeft `nonIntRem` evaluatedRight

-- ─── NON INTEGER REMAINING ──────────────────────────────────────────────────────

nonIntRem :: Double -> Double -> Double
nonIntRem x y =
    x - (y * (fromIntegral $ truncate (x/y)))

-- ────────────────────────────────────────────────────────────────────────────────
