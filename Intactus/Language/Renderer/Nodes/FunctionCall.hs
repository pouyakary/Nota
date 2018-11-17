

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

renderSimpleFunction :: String -> [ AST ] -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderSimpleFunction name args render = result where
    boxedName =
        spacedBox name
    boxedArgs =
        functionArgsConcat [ render x False | x <- args ]
    parenthesisedArgs =
        createBracketWithStyle Bracket boxedArgs
    result =
        verticalConcat [ boxedName, parenthesisedArgs ]

functionArgsConcat :: [ SpacedBox ] -> SpacedBox
functionArgsConcat boxes = SpacedBox { boxLines = resultLines
                                     , width    = resultWidth
                                     , height   = resultHeight
                                     , baseLine = resultBaseLine
                                     }
    where
        resultHeight =
            maximum [ length ( boxLines x ) | x <- boxes ]
        centeredBoxlines =
            [ centerText ( width x ) resultHeight x | x <- boxes ]
        resultWidth =
            sum [ 2 + width x | x <- boxes ] - 2
        resultLines =
            [ intercalate ( connectorOfLine lineNumber ) [ ( boxLines x ) !! lineNumber
                | x <- centeredBoxlines ]
                    | lineNumber <- [ 0.. ( resultHeight - 1 ) ] ]
        resultCenter =
            resultHeight `div` 2
        connectorOfLine x =
            if x == resultCenter then ", " else "  "
        resultBaseLine =
            if odd resultHeight
                then resultHeight `div` 2
                else resultHeight `div` 2

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
