
module Infrastructure.Text.Shapes.Types where

-- ─── BOX TYPE ───────────────────────────────────────────────────────────────────

data BoxType = Bracket
             | Absolute
             | Floor
             | Ceiling
             | Parenthesis
             | LightBox
             | Corners

-- ─── BOX CHAR SET ───────────────────────────────────────────────────────────────

data BoxCharSet = BoxCharSet { boxTopLeft     :: Char
                             , boxTop         :: Char
                             , boxTopRight    :: Char
                             , boxRight       :: Char
                             , boxBottomRight :: Char
                             , boxBottom      :: Char
                             , boxBottomLeft  :: Char
                             , boxLeft        :: Char
                             }

-- ────────────────────────────────────────────────────────────────────────────────
