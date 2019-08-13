
module Language.BackEnd.Renderer.Main ( render ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.AST
import Language.BackEnd.Renderer.Nodes.Assignment
import Language.BackEnd.Renderer.Nodes.BinaryOperator
import Language.BackEnd.Renderer.Nodes.FunctionCall
import Language.BackEnd.Renderer.Nodes.Identifier
import Language.BackEnd.Renderer.Nodes.Number
import Language.BackEnd.Renderer.Nodes.Parenthesis
import Language.BackEnd.Renderer.Nodes.Versus
import Language.BackEnd.Renderer.Nodes.Negation

-- ─── RENDER BASE ────────────────────────────────────────────────────────────────

render :: AST -> Bool -> SpacedBox
render node ambiguous =
    case node of
        ASTNumber x ->
            renderASTNumber x
        ASTIdentifier x ->
            renderASTIdentifer x
        ASTBinaryOperator op left right ->
            renderASTBinaryOperator op left right render
        ASTFunctionCall name args ->
            renderASTFunctionCall name args render
        ASTVersus parts ->
            renderASTVersus parts render
        ASTAssignment name value ->
            renderASTAssignment name value render
        ASTNegation x ->
            renderASTNegation x render
        ASTParenthesis child ->
            renderASTParenthesis child ambiguous render

-- ────────────────────────────────────────────────────────────────────────────────
