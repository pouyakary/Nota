
module Language.BackEnd.Evaluator.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.AST
import Model

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

type EvalResult = Either String P50

type LeafEvalSignature = AST -> ScopePrototype -> EvalResult

type StemEvalSignature = ( LeafEvalSignature ) -> LeafEvalSignature

-- ────────────────────────────────────────────────────────────────────────────────
