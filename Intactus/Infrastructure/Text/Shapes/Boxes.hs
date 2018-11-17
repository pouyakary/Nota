
module Infrastructure.Text.Shapes.Boxes where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Shapes.Types
import Infrastructure.Text.Shapes.Presets
import Infrastructure.Text.Tools
import Infrastructure.Text.Layout

-- ─── CREATE BOX ─────────────────────────────────────────────────────────────────

shapeBox :: BoxType -> SpacedBox -> SpacedBox
shapeBox boxType content =
    SpacedBox { boxLines = result
              , width    = ( width content ) + 4
              , height   = length result
              , baseLine = 1 + baseLine content
              }
    where
        charSet =
            boxCharsOfType boxType
        firstLine =
            [ boxTopLeft charSet ] ++
            ( repeatText ( boxTop charSet ) ( ( width content ) + 2 ) ) ++
            [ boxTopRight charSet]
        middleLines =
            [ createMiddleLine line | line <- boxLines content ]
        createMiddleLine line =
            [ boxLeft charSet ] ++ " " ++ line ++ " " ++ [ boxRight charSet ]
        lastLine =
            [ boxBottomLeft charSet ] ++
            ( repeatText ( boxBottom charSet ) ( ( width content ) + 2 ) ) ++
            [ boxBottomRight charSet ]
        result =
            [ firstLine ] ++ middleLines ++ [ lastLine ]


-- ────────────────────────────────────────────────────────────────────────────────
