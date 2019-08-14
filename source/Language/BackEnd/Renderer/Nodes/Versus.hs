

module Language.BackEnd.Renderer.Nodes.Versus ( renderASTVersus ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.AST

-- ─── VERSUS SIGN ────────────────────────────────────────────────────────────────

versusSign = SpacedBox { boxLines = [ " • ", " • ", " • " ]
                       , height   = 3
                       , width    = 3
                       , baseLine = 1
                       }

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTVersus :: [ AST ] -> ( AST -> Bool -> SpacedBox ) -> SpacedBox
renderASTVersus parts render = result where
    result =
        case length parts of
            0 -> spacedBox "✨"
            1 -> render ( parts !! 0 ) False
            _ -> horizontalConcat $ computeSignedParts parts
    computeSignedParts xs =
        ( render ( head xs ) False ) : concat [ [ versusSign, render x False ]
                                                | x <- tail xs ]

-- ────────────────────────────────────────────────────────────────────────────────