
module Language.BackEnd.Evaluator.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.AST
import Data.Scientific
import Model

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

type EvalResult = Either String Scientific

type EvalSignature = AST -> ScopePrototype -> EvalResult

-- ────────────────────────────────────────────────────────────────────────────────
