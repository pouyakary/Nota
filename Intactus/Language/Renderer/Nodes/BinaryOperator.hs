

module Language.Renderer.Nodes.BinaryOperator ( renderASTBinaryOperator ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout
import Infrastructure.Text.Tools

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTBinaryOperator :: BinaryOperators -> AST -> AST -> ( AST -> SpacedBox ) -> SpacedBox

-- ─── DIVISION ───────────────────────────────────────────────────────────────────

renderASTBinaryOperator Div left right render = result where
    result = SpacedBox { boxLines = lines
                       , width    = boxWidth
                       , height   = boxHeight
                       }
    lines =
        ( boxLines renderedLeft ) ++ [ divisionLine ] ++ ( boxLines renderedRight )
    divisionLine =
        repeatText '─' boxWidth
    upperBox =
        centerText ( width renderedLeft ) boxHeight renderedLeft
    lowerBox =
        centerText ( height renderedRight ) boxHeight renderedRight
    boxHeight =
        height renderedRight + height renderedLeft + 1
    boxWidth =
        maximum [ width renderedLeft, width renderedRight ]
    renderedLeft =
        render left
    renderedRight =
        render right


-- ─── GENERAL RULE ───────────────────────────────────────────────────────────────

renderASTBinaryOperator op left right render =
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
