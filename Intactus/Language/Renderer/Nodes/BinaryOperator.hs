

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
                       , baseLine = height renderedLeft
                       }
    lines =
        ( boxLines renderedLeft ) ++ [ divisionLine ] ++ ( boxLines renderedRight )
    divisionLine =
        repeatText '─' boxWidth
    upperBox =
        centerText boxWidth ( height renderedLeft ) renderedLeft
    lowerBox =
        centerText boxWidth ( height renderedRight ) renderedRight
    boxHeight =
        height renderedRight + height renderedLeft + 1
    boxWidth =
        maximum [ width renderedLeft, width renderedRight ]
    renderedLeft =
        render left
    renderedRight =
        render right

-- ─── POWER OPERATOR ─────────────────────────────────────────────────────────────

renderASTBinaryOperator Pow left right render = result where
    result =
        if height renderedLeft < height renderedRight
            then renderPowWithRightAsMax renderedLeft renderedRight
            else renderPowWithLeftAsMax  renderedLeft renderedRight
    renderedRight =
        render right
    renderedLeft =
        render left
    renderPowWithRightAsMax left right =
        result where
            result =
                verticalConcatWithoutSpace [ marginedLeft, marginedRight ]
            marginedLeft =
                marginedBox leftMargin left
            marginedRight =
                marginedBox rightMargin right
            rightMargin =
                BoxSize 0 0 ( height left - height right + 1 ) 0
            leftMargin =
                BoxSize 1 0 0 0
    renderPowWithLeftAsMax left right =
        verticalConcatWithoutSpace [ left, right ]

-- ─── GENERAL RULE ───────────────────────────────────────────────────────────────

renderASTBinaryOperator op left right render =
    verticalConcat boxes where
        boxes =
            [ render left, operatorBox, render right ]
        operatorBox =
            spacedBox opString
        opString =
            case op of Sum -> "+"
                       Sub -> "-"
                       Mul -> "×"
                       Mod -> "%"
                       Equ -> "?"
                       NEq -> "!"

-- ────────────────────────────────────────────────────────────────────────────────
