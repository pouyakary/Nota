
module Language.Executioner.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Language.Executioner.Types
import Data.Scientific

-- ─── MAIN ───────────────────────────────────────────────────────────────────────

compute :: AST -> ScopePrototype -> [ Scientific ] -> Scientific
compute astNode scopePrototype functionsParams =
    0

-- ────────────────────────────────────────────────────────────────────────────────
