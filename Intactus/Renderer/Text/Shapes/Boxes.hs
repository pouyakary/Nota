
module Renderer.Text.Shapes.Boxes where

import Data.List
import Renderer.Text.Tools
import Renderer.Text.Layout

-- ─── BOX TYPES ──────────────────────────────────────────────────────────────────

data BoxType =
   Bracket | Absolute | Floor | Ceiling

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

-- ─── CREATE BOX ─────────────────────────────────────────────────────────────────

shapeBox :: BoxType -> SpacedBox -> SpacedBox
shapeBox boxType content =
   SpacedBox { boxLines = result
             , width    = size + 4
             , height   = length result
             }
    where
        size =
            width content
        charSet =
            boxCharsOfType boxType
        firstLine =
            [ boxTopLeft charSet ] ++
            ( repeatText ( boxTop charSet ) ( size + 2 ) ) ++
            [ boxTopRight charSet]
        middleLines =
            [ [ boxLeft charSet ] ++ " " ++ line ++
            " " ++ [ boxRight charSet ]
            | line <- boxLines content ]
        lastLine =
            [ boxBottomLeft charSet ] ++
            ( repeatText ( boxBottom charSet ) ( size + 2 ) ) ++
            [ boxBottomRight charSet ]
        result =
            [ firstLine ] ++ middleLines ++ [ lastLine ]


-- ────────────────────────────────────────────────────────────────────────────────
