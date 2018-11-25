
module Infrastructure.Text.Shapes.Presets where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Shapes.Types

-- ─── TYPE ───────────────────────────────────────────────────────────────────────

boxCharsOfType :: BoxType -> BoxCharSet

-- ─── BRACKET ────────────────────────────────────────────────────────────────────

boxCharsOfType Bracket =
    BoxCharSet { boxTopLeft     = '┌'
               , boxTop         = ' '
               , boxTopRight    = '┐'
               , boxRight       = '│'
               , boxBottomRight = '┘'
               , boxBottom      = ' '
               , boxBottomLeft  = '└'
               , boxLeft        = '│'
               }

-- ─── ABSOLUTE ───────────────────────────────────────────────────────────────────

boxCharsOfType Absolute =
    BoxCharSet { boxTopLeft     = '⎢'
               , boxTop         = ' '
               , boxTopRight    = '⎥'
               , boxRight       = '⎥'
               , boxBottomRight = '⎥'
               , boxBottom      = ' '
               , boxBottomLeft  = '⎢'
               , boxLeft        = '⎢'
               }

-- ─── FLOOR ──────────────────────────────────────────────────────────────────────

boxCharsOfType Floor =
    BoxCharSet { boxTopLeft     = '⎜'
               , boxTop         = ' '
               , boxTopRight    = '⎟'
               , boxRight       = '⎟'
               , boxBottomRight = '⎦'
               , boxBottom      = ' '
               , boxBottomLeft  = '⎣'
               , boxLeft        = '⎜'
               }

-- ─── CEILING ────────────────────────────────────────────────────────────────────

boxCharsOfType Ceiling =
    BoxCharSet { boxTopLeft     = '⎡'
               , boxTop         = ' '
               , boxTopRight    = '⎤'
               , boxRight       = '⎟'
               , boxBottomRight = '⎥'
               , boxBottom      = ' '
               , boxBottomLeft  = '⎢'
               , boxLeft        = '⎢'
               }

-- ─── PARENTHESIS ────────────────────────────────────────────────────────────────

boxCharsOfType Parenthesis =
    BoxCharSet { boxTopLeft     = '⎛'
               , boxTop         = ' '
               , boxTopRight    = '⎞'
               , boxRight       = '⎟'
               , boxBottomRight = '⎠'
               , boxBottom      = ' '
               , boxBottomLeft  = '⎝'
               , boxLeft        = '⎜'
               }

-- ─── LIGHT BOX ──────────────────────────────────────────────────────────────────

boxCharsOfType LightBox =
    BoxCharSet { boxTopLeft     = '┌'
               , boxTop         = '─'
               , boxTopRight    = '┐'
               , boxRight       = '│'
               , boxBottomRight = '┘'
               , boxBottom      = '─'
               , boxBottomLeft  = '└'
               , boxLeft        = '│'
               }

-- ─── CORNERS ────────────────────────────────────────────────────────────────────

boxCharsOfType Corners =
    BoxCharSet { boxTopLeft     = '┌'
               , boxTop         = ' '
               , boxTopRight    = '┐'
               , boxRight       = ' '
               , boxBottomRight = '┘'
               , boxBottom      = ' '
               , boxBottomLeft  = '└'
               , boxLeft        = ' '
               }

-- ────────────────────────────────────────────────────────────────────────────────
