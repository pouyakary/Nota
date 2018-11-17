
module Language.Renderer.Main ( render ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.Types
import Language.Renderer.Nodes.Assignment
import Language.Renderer.Nodes.BinaryOperator
import Language.Renderer.Nodes.FunctionCall
import Language.Renderer.Nodes.Identifier
import Language.Renderer.Nodes.Number
import Language.Renderer.Nodes.Parenthesis
import Language.Renderer.Nodes.Versus

-- ─── RENDER BASE ────────────────────────────────────────────────────────────────

render :: AST -> Bool -> SpacedBox
render node ambiguous =
    case node of
        ASTNumber x ->
            renderASTNumber x
        ASTIdentifer x ->
            renderASTIdentifer x
        ASTBinaryOperator op left right ->
            renderASTBinaryOperator op left right render
        ASTFunctionCall name args ->
            renderASTFunctionCall name args render
        ASTVersus parts ->
            renderASTVersus parts render
        ASTAssignment name value ->
            renderASTAssignment name value render
        ASTParenthesis child ->
            renderASTParenthesis child ambiguous render
        _ ->
            spacedBox $ show node

-- ────────────────────────────────────────────────────────────────────────────────
