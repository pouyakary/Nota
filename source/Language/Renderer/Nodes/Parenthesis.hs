
module Language.Renderer.Nodes.Parenthesis ( renderASTParenthesis ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Brackets
import Infrastructure.Text.Shapes.Types
import Language.FrontEnd.AST

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTParenthesis :: AST -> Bool -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTParenthesis node ambiguous render = result where
    result = if ambiguous
                then createBracketWithStyle Bracket renderedNode
                else renderedNode
    renderedNode =
        render node False

-- ────────────────────────────────────────────────────────────────────────────────