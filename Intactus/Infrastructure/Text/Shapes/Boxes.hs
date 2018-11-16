
module Infrastructure.Text.Shapes.Boxes where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Tools
import Infrastructure.Text.Layout

-- ─── BOX TYPES ──────────────────────────────────────────────────────────────────

data BoxType = Bracket | Absolute | Floor | Ceiling | Parenthesis | LightBox | Corners

data BoxCharSet = BoxCharSet { boxTopLeft     :: Char
                             , boxTop         :: Char
                             , boxTopRight    :: Char
                             , boxRight       :: Char
                             , boxBottomRight :: Char
                             , boxBottom      :: Char
                             , boxBottomLeft  :: Char
                             , boxLeft        :: Char
                             }


-- ─── GET BOX CHARACTERS ─────────────────────────────────────────────────────────

boxCharsOfType :: BoxType -> BoxCharSet
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
