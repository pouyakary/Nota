

module Language.Renderer.Nodes.FunctionCall ( renderASTFunctionCall ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Brackets
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Presets
import Infrastructure.Text.Shapes.Types
import Language.FrontEnd.Types

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTFunctionCall :: AST -> [ AST ] -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTFunctionCall ( ASTIdentifer name ) args render = result where
    result =
        if length args == 1
            then specialFunctionsResult
            else renderSimpleFunction name args render
    specialFunctionsResult =
        case name of
            "abs"     -> renderAbsoluteFunction ( args !! 0 ) render
            "floor"   -> renderFloorFunction    ( args !! 0 ) render
            "ceiling" -> renderCeilingFunction  ( args !! 0 ) render
            _         -> renderSimpleFunction   name args render

-- ─── RENDER SIMPLE FUNCTION ─────────────────────────────────────────────────────

-- for less computation
emptyFunctionBracket =
    spacedBox "┌  ┐\n│  │\n└  ┘"

renderSimpleFunction :: String -> [ AST ] -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderSimpleFunction name args render = result where
    boxedName =
        spacedBox name
    renderedArgs =
        [ render x False | x <- args ]
    parenthesisedArgs =
        if length renderedArgs == 0
            then emptyFunctionBracket
            else createBracketWithStyle Bracket boxedArgs
        where
            comma =
                spacedBox ","
            boxedArgs =
                baselineVerticalConcat $ argsHead ++ argsTail
            argsHead =
                [ head renderedArgs ]
            argsTail =
                concat [ [ comma, x ] | x <- tail renderedArgs ]
    result =
        baselineVerticalConcat [ boxedName, parenthesisedArgs ]

-- ─── GENERAL BOXED TYPE FUNCTION RENDERER ───────────────────────────────────────

renderGeneralBoxFunction :: String -> String -> BoxType -> AST -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderGeneralBoxFunction left right boxType child render = result where
    result =
        if height renderedChild == 1
            then verticalConcat [ spacedBox left, renderedChild, spacedBox right ]
            else createBracketWithStyle boxType renderedChild
    renderedChild =
        render child False

-- ─── ABSOLUTE ───────────────────────────────────────────────────────────────────

renderAbsoluteFunction =
    renderGeneralBoxFunction "|" "|" Absolute

-- ─── FLOOR ──────────────────────────────────────────────────────────────────────

renderFloorFunction =
    renderGeneralBoxFunction "⎣" "⎦" Floor

-- ─── CEILING ────────────────────────────────────────────────────────────────────

renderCeilingFunction =
    renderGeneralBoxFunction "⎡" "⎤" Ceiling

-- ────────────────────────────────────────────────────────────────────────────────
