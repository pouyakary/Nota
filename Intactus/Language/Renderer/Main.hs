
module Language.Renderer.Main ( render, renderASTtoString ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.Types
import Language.Renderer.Nodes.Number
import Language.Renderer.Nodes.Identifier
import Language.Renderer.Nodes.BinaryOperator

-- ─── MAIN API ───────────────────────────────────────────────────────────────────

renderASTtoString :: AST -> String
renderASTtoString node =
    spacedBoxToString $ render node

-- ─── RENDER BASE ────────────────────────────────────────────────────────────────

render :: AST -> SpacedBox
render node =
    case node of
        ASTNumber x ->
            rendereASTNumber x
        ASTIdentifer x ->
            rendereASTIdentifer x
        ASTBinaryOperator op left right ->
            rendereASTBinaryOperator op left right render
        _ ->
            spacedBox $ show node

-- ────────────────────────────────────────────────────────────────────────────────
