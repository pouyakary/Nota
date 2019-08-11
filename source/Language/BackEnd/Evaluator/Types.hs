
module Language.BackEnd.Evaluator.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.AST
import Model

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

type EvalResult = Either String Double

type LeafEvalSignature = AST -> ScopePrototype -> EvalResult

type StemEvalSignature = ( LeafEvalSignature ) -> LeafEvalSignature

type MasterEvalResult = Either String MasterEvalResultRight

data MasterEvalResultRight =
    MasterEvalResultRight [ Double ] Model

-- ────────────────────────────────────────────────────────────────────────────────
