

module Language.BackEnd.Renderer.Nodes.Negation ( renderASTNegation) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.AST

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTNegation :: AST -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTNegation x render =
    case x of
        ASTIdentifier _ ->
            horizontalConcatWithoutSpace [ negation, renderedChild ]
        ASTNumber _ ->
            horizontalConcatWithoutSpace [ negation, renderedChild ]
        _ ->
            horizontalConcat [ negation, renderedChild ]

    where
        negation =
            spacedBox "-"
        renderedChild =
            render x True

-- ────────────────────────────────────────────────────────────────────────────────
