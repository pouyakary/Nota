

module Language.BackEnd.Renderer.Nodes.Assignment ( renderASTAssignment ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.AST

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTAssignment :: AST -> AST -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTAssignment ( ASTIdentifier name ) value render = result where
    result =
        horizontalConcat [ renderedName, renderedValue ]
    renderedName =
        spacedBox $ "⟨" ++ name ++ "⟩ ≡ "
    renderedValue =
        render value False

-- ────────────────────────────────────────────────────────────────────────────────