

module Language.Renderer.Nodes.Assignment ( renderASTAssignment ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.Types

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTAssignment :: AST -> AST -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTAssignment name value render = result where
    result =
        verticalConcat [ renderedName, assignSign, renderedValue ]
    renderedName =
        render name False
    assignSign =
        spacedBox " = "
    renderedValue =
        render value False

-- ────────────────────────────────────────────────────────────────────────────────