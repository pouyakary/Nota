

module Language.Renderer.Nodes.Assignment ( renderASTAssignment ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTAssignment :: AST -> AST -> ( AST -> SpacedBox ) -> SpacedBox
renderASTAssignment name value render = result where
    result =
        baselineVerticalConcat [ renderedName, assignSign, renderedValue ]
    renderedName =
        render name
    assignSign =
        spacedBox "="
    renderedValue =
        render value

-- ────────────────────────────────────────────────────────────────────────────────