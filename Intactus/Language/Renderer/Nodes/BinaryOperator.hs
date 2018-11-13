

module Language.Renderer.Nodes.BinaryOperator ( rendereASTBinaryOperator ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

rendereASTBinaryOperator :: BinaryOperators -> AST -> AST -> ( AST -> SpacedBox ) -> SpacedBox
rendereASTBinaryOperator op left right render =
    verticalConcat boxes where
        boxes =
            [ render left, operatorBox, render right ]
        operatorBox =
            spacedBox opString
        opString =
            case op of Div -> "/"
                       Sum -> "+"
                       Sub -> "-"
                       Mul -> "*"
                       Mod -> "%"
                       Pow -> "^"
                       Equ -> "?"
                       NEq -> "!"


-- ────────────────────────────────────────────────────────────────────────────────
