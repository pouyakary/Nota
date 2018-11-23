

module Language.Renderer.Nodes.Assignment ( renderASTAssignment ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.Types
import Language.Renderer.Nodes.Identifier

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTAssignment :: AST -> AST -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTAssignment ( ASTIdentifier name ) value render = result where
    result =
        verticalConcat [ renderedName, renderedValue ]
    renderedName =
        spacedBox $ "⟨" ++ identifierPrettyName name ++ "⟩ ≡ "
    renderedValue =
        render value False

-- ────────────────────────────────────────────────────────────────────────────────