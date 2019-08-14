
module Language.BackEnd.Evaluator.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.AST
import Model

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

type EvalResult = Either String Double

type EvalFunction = AST -> ScopePrototype -> EvalResult

type LeafEvalSignature = EvalFunction

type StemEvalSignature = ( LeafEvalSignature ) -> LeafEvalSignature

type MasterEvalResult = Either String MasterEvalResultRight

data MasterEvalResultRight =
    MasterEvalResultRight [ Double ] Model

-- ────────────────────────────────────────────────────────────────────────────────
