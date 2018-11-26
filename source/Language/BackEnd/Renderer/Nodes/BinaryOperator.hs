

module Language.BackEnd.Renderer.Nodes.BinaryOperator ( renderASTBinaryOperator ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Infrastructure.Text.Tools
import Language.FrontEnd.AST

-- ─── TYPES ──────────────────────────────────────────────────────────────────────

type Renderer = AST -> Bool -> SpacedBox

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTBinaryOperator :: BinaryOperators -> AST -> AST -> Renderer -> SpacedBox
renderASTBinaryOperator op left right renderNode =
    case op of Div -> renderDivisionOperator    left right renderNode
               Pow -> renderPowerOperator       left right renderNode
               _   -> renderNormalOperators  op left right renderNode

-- ─── DIVISION ───────────────────────────────────────────────────────────────────

renderDivisionOperator :: AST -> AST -> Renderer -> SpacedBox
renderDivisionOperator left right renderNode = result where
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
        renderNode left False
    renderedRight =
        renderNode right False

-- ─── POWER OPERATOR ─────────────────────────────────────────────────────────────

renderPowerOperator :: AST -> AST -> Renderer -> SpacedBox
renderPowerOperator left right renderNode = result where
    renderedLeft =
        renderNode left False
    renderedRight =
        renderNode right False
    isLeftPow =
        case left of ASTBinaryOperator Pow _ _ -> True
                     _                         -> False
    glueSpacing =
        case left of ASTFunctionCall _ _ -> " "
                     _                   -> ""
    heightReduction =
        if not isLeftPow && height renderedLeft > 2 then 1 else 0
    leftPartLines =
        boxLines $ marginedBox ( BoxSize ( height renderedRight - heightReduction ) 0 0 0 ) renderedLeft
    rightPartLines =
        boxLines $ marginedBox ( BoxSize 0 0 ( height renderedLeft - heightReduction ) 0 ) renderedRight
    resultHeight =
        height renderedLeft + height renderedRight - heightReduction
    resultWidth =
        width renderedLeft + width renderedRight + length glueSpacing
    resultLines =
        [ ( leftPartLines !! x ) ++ glueSpacing ++ ( rightPartLines !! x )
            | x <- [ 0 .. resultHeight - 1 ] ]
    resultBaseline =
        baseLine renderedLeft + height renderedRight - heightReduction
    result =
        SpacedBox { boxLines = resultLines
                  , width    = resultWidth
                  , height   = resultHeight
                  , baseLine = resultBaseline
                  }

-- ─── GENERAL RULE ───────────────────────────────────────────────────────────────

renderNormalOperators :: BinaryOperators -> AST -> AST -> Renderer -> SpacedBox
renderNormalOperators op left right renderNode =
    verticalConcat boxes where
        boxes =
            [ renderNode left True, operatorBox, renderNode right True ]
        operatorBox =
            spacedBox opString
        opString =
            case op of Sum -> "+"
                       Sub -> "-"
                       Mul -> "×"
                       Mod -> "%"
                       Equ -> "="
                       NEq -> "≠"

-- ────────────────────────────────────────────────────────────────────────────────
