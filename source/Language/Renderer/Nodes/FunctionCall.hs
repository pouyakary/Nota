

module Language.Renderer.Nodes.FunctionCall ( renderASTFunctionCall ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Brackets
import Infrastructure.Text.Shapes.Presets
import Infrastructure.Text.Shapes.Types
import Infrastructure.Text.Tools
import Language.FrontEnd.AST

-- ─── TYPES ──────────────────────────────────────────────────────────────────────

type Renderer = AST -> Bool -> SpacedBox

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTFunctionCall :: AST -> [ AST ] -> Renderer -> SpacedBox
renderASTFunctionCall ( ASTIdentifier name ) args render = result where
    result =
        if length args == 1 then specialFunctionsResult
                            else renderSimpleFunction name args render
    specialFunctionsResult =
        case name of
            "Abs"          ->  renderAbsoluteFunction    arg         render
            "Floor"        ->  renderFloorFunction       arg         render
            "Ceiling"      ->  renderCeilingFunction     arg         render
            "Sqrt"         ->  renderSquareRootFunction  arg         render
            _              ->  renderSimpleFunction      name  args  render
            where
                arg = args !! 0

-- ─── RENDER SIMPLE FUNCTION ─────────────────────────────────────────────────────

-- for less computation
emptyFunctionBracket =
    spacedBox "┌  ┐\n│  │\n└  ┘"

renderSimpleFunction :: String -> [ AST ] -> Renderer -> SpacedBox
renderSimpleFunction name args render = result where
    boxedName =
        spacedBox name
    renderedArgs =
        [ render x False | x <- args ]
    parenthesisedArgs =
        if length renderedArgs == 0 then emptyFunctionBracket
                                    else createBracketWithStyle Bracket boxedArgs
        where
            comma =
                spacedBox ","
            boxedArgs =
                verticalConcat $ argsHead ++ argsTail
            argsHead =
                [ head renderedArgs ]
            argsTail =
                concat [ [ comma, x ] | x <- tail renderedArgs ]
    result =
        verticalConcat [ boxedName, parenthesisedArgs ]

-- ─── GENERAL BOXED TYPE FUNCTION RENDERER ───────────────────────────────────────

renderGeneralBoxFunction :: String -> String -> BoxType -> AST -> Renderer -> SpacedBox
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

-- ─── RENDER SQUARE ROOT ─────────────────────────────────────────────────────────

renderSquareRootFunction :: AST -> Renderer -> SpacedBox
renderSquareRootFunction child render = result where
    renderedChild =
        marginedBox ( BoxSize 0 1 0 1 ) $ render child False
    leftPart =
        [ " " | _ <- [ 1 .. height renderedChild ] ] ++ [ "╲" ]
    middlePartLineForPosition x =
        leftSpacing ++ "╱" ++ rightSpacing where
            size =
                height renderedChild
            leftSpacing =
                [ ' ' | _ <- [ x .. size - 2 ] ]
            rightSpacing =
                [ ' ' | _ <- [ 1 .. x ] ]
    middlePart =
        [ repeatText ' ' $ height renderedChild ] ++
        [ middlePartLineForPosition x | x <- [ 0 .. height renderedChild - 1 ] ]
    rightPart =
        [ repeatText '_' $ width renderedChild ] ++ boxLines renderedChild
    resultLines =
        [ ( leftPart !! x ) ++ ( middlePart !! x ) ++ ( rightPart !! x )
            | x <- [ 0 .. height renderedChild ] ]
    result =
        SpacedBox { boxLines = resultLines
                  , width    = 1 + height   renderedChild + width renderedChild
                  , height   = 1 + height   renderedChild
                  , baseLine = 1 + baseLine renderedChild
                  }

-- ────────────────────────────────────────────────────────────────────────────────
