

module Language.Renderer.Nodes.Assignment ( renderASTAssignment ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTAssignment :: AST -> AST -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTAssignment name value render = result where
    result =
        baselineVerticalConcat [ renderedName, assignSign, renderedValue ]
    renderedName =
        render name False
    assignSign =
        spacedBox "="
    renderedValue =
        render value False

-- ────────────────────────────────────────────────────────────────────────────────