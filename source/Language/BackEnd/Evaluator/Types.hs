
module Language.Evaluator.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Scientific
import Model

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

type EvalResult = Either String Scientific

type EvalSignature = AST -> ScopePrototype -> [ Scientific ] -> EvalResult

-- ────────────────────────────────────────────────────────────────────────────────
