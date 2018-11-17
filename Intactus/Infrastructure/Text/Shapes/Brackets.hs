
module Infrastructure.Text.Shapes.Brackets where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Shapes.Types
import Infrastructure.Text.Shapes.Presets
import Infrastructure.Text.Tools
import Infrastructure.Text.Layout

-- ─── CREATE BRACKET ─────────────────────────────────────────────────────────────

createBracketWithStyle :: BoxType -> SpacedBox -> SpacedBox
createBracketWithStyle style box = result where
    charset =
        boxCharsOfType style
    boxHeight =
        height box
    baseBracketSize
        | boxHeight <= 3 =
            3
        | boxHeight > 3 && boxHeight <= 5 =
            boxHeight
        | otherwise =
            floor $ 0.6 * fromIntegral boxHeight
    bracketSize =
        if odd $ boxHeight - baseBracketSize
            then baseBracketSize + 1
            else baseBracketSize
    leftBraceLines =
        [ [ boxTopLeft     charset ] ] ++
        [ [ boxLeft        charset ] | _ <- [ 3 .. bracketSize ] ] ++
        [ [ boxBottomLeft  charset ] ]
    rightBraceLines =
        [ [ boxTopRight    charset ] ] ++
        [ [ boxRight       charset ] | _ <- [ 3 .. bracketSize ] ] ++
        [ [ boxBottomRight charset ] ]
    leftBrace =
        SpacedBox { boxLines = leftBraceLines
                  , height   = bracketSize
                  , width    = 1
                  , baseLine = bracketSize `div` 2
                  }
    rightBrace =
        SpacedBox { boxLines = rightBraceLines
                  , height   = bracketSize
                  , width    = 1
                  , baseLine = bracketSize `div` 2
                  }
    result =
        verticalConcat [ leftBrace, box, rightBrace ]

-- ────────────────────────────────────────────────────────────────────────────────
