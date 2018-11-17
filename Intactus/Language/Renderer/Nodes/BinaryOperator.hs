

module Language.Renderer.Nodes.BinaryOperator ( renderASTBinaryOperator ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Infrastructure.Text.Tools
import Language.FrontEnd.Types

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTBinaryOperator :: BinaryOperators -> AST -> AST -> ( AST -> Bool -> SpacedBox ) -> SpacedBox

-- ─── DIVISION ───────────────────────────────────────────────────────────────────

renderASTBinaryOperator Div left right renderNode = result where
    result = SpacedBox { boxLines = lines
                       , width    = boxWidth
                       , height   = boxHeight
                       , baseLine = height renderedLeft
                       }
    lines =
        ( boxLines upperBox ) ++ [ divisionLine ] ++ ( boxLines lowerBox )
    divisionLine =
        repeatText '─' boxWidth
    upperBox =
        centerText boxWidth ( height renderedLeft ) renderedLeft
    lowerBox =
        centerText boxWidth ( height renderedRight ) renderedRight
    boxHeight =
        ( height renderedRight ) + ( height renderedLeft ) + 1
    boxWidth =
        2 + maximum [ width renderedLeft, width renderedRight ]
    renderedLeft =
        renderNode left True
    renderedRight =
        renderNode right True

-- ─── POWER OPERATOR ─────────────────────────────────────────────────────────────

renderASTBinaryOperator Pow left right renderNode = result where
    result =
        if height renderedLeft < height renderedRight
            then renderPowWithRightAsMax renderedLeft renderedRight
            else renderPowWithLeftAsMax  renderedLeft renderedRight
    renderedRight =
        renderNode right True
    renderedLeft =
        renderNode left True
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

renderASTBinaryOperator op left right renderNode =
    baselineVerticalConcat boxes where
        boxes =
            [ renderNode left True, operatorBox, renderNode right True ]
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
