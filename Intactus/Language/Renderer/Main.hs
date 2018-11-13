
module Language.Renderer.Main ( render ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Language.Renderer.Nodes.Number

-- ─── MAIN API ───────────────────────────────────────────────────────────────────

render :: AST -> String
render node =
    case node of
        ASTNumber x -> rendereASTNumber x
        _ -> show node

-- ────────────────────────────────────────────────────────────────────────────────
