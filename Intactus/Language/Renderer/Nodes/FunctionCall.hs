

module Language.Renderer.Nodes.FunctionCall ( renderASTFunctionCall ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Brackets
import Data.List

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTFunctionCall :: AST -> [ AST ] -> ( AST -> SpacedBox ) -> SpacedBox
renderASTFunctionCall ( ASTIdentifer name ) args render =
    case name of
        _ -> renderSimpleFunction name args render

-- ─── RENDER SIMPLE FUNCTION ─────────────────────────────────────────────────────

renderSimpleFunction :: String -> [ AST ] -> ( AST -> SpacedBox ) -> SpacedBox
renderSimpleFunction name args render = result where
    boxedName =
        spacedBox name
    boxedArgs =
        functionArgsConcat [ render x | x <- args ]
    parenthesisedArgs =
        createBracketWithStyle BracketBracket boxedArgs
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
            [ intercalate ( connectorOfLine lineNumber ) [ ( boxLines x ) !! lineNumber | x <- centeredBoxlines ]
                | lineNumber <- [ 0.. ( resultHeight - 1 ) ] ]
        resultCenter =
            resultHeight `div` 2
        connectorOfLine x =
            if x == resultCenter then ", " else "  "
        resultBaseLine =
            if odd resultHeight
                then resultHeight `div` 2
                else resultHeight `div` 2

-- ────────────────────────────────────────────────────────────────────────────────
