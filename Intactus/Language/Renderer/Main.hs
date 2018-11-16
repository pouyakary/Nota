
module Language.Renderer.Main ( render ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.Types
import Language.Renderer.Nodes.Number
import Language.Renderer.Nodes.Identifier
import Language.Renderer.Nodes.BinaryOperator


-- ─── RENDER BASE ────────────────────────────────────────────────────────────────

render :: AST -> SpacedBox
render node =
    case node of
        ASTNumber x ->
            renderASTNumber x
        ASTIdentifer x ->
            renderASTIdentifer x
        ASTBinaryOperator op left right ->
            renderASTBinaryOperator op left right render
        _ ->
            spacedBox $ show node

-- ────────────────────────────────────────────────────────────────────────────────
