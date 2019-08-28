
module Infrastructure.Text.Shapes.Table where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Presets
import Infrastructure.Text.Shapes.Types
import Infrastructure.Text.Tools

-- ─── SIMPLE TABLE ───────────────────────────────────────────────────────────────

numbersRowTable :: [ Double ] -> SpacedBox
numbersRowTable numbers =
    horizontalConcatCore "" sideBoxes where
        sideBoxes =
            [ leftSide ] ++ boxes ++ [ rightSide ]
        leftSide =
            spacedBox "┌─\n│ \n└─"
        rightSide =
            spacedBox "─┐\n │\n─┘"
        spacer =
            spacedBox "─┬─\n │ \n─┴─"
        boxes =
            putBoxesInBetween spacer [ renderNumberBox x | x <- numbers ]
        renderNumberBox number =
            numberBox where
                numberBox =
                    spacedBox $ line ++ "\n" ++ renderedNumber ++ "\n" ++ line
                renderedNumber =
                    show number
                line =
                    intercalate "" [ "─" | x <- renderedNumber ]
        putBoxesInBetween between list =
            case length list of
                1 -> list
                _ -> [ head list, between ] ++ ( putBoxesInBetween between ( tail list ) )


-- ────────────────────────────────────────────────────────────────────────────────
