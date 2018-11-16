
module Infrastructure.Text.Shapes.Brackets where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Tools
import Infrastructure.Text.Layout

-- ─── TYPES ──────────────────────────────────────────────────────────────────────

data BracketStlye = ParenthesisBracket | BracketBracket

data BracketCharset = BracketCharset { bracketTopLeft         :: String
                                     , bracketLeft            :: String
                                     , bracketBottomLeft      :: String
                                     , bracketTopRight        :: String
                                     , bracketRight           :: String
                                     , bracketBottomRight     :: String
                                     }

-- ─── STYLES ─────────────────────────────────────────────────────────────────────

parenthesisCharsOfType :: BracketStlye -> BracketCharset

parenthesisCharsOfType BracketBracket =
    BracketCharset { bracketTopLeft         = "┌"
                   , bracketLeft            = "│"
                   , bracketBottomLeft      = "└"
                   , bracketTopRight        = "┐"
                   , bracketRight           = "│"
                   , bracketBottomRight     = "┘"
                   }

parenthesisCharsOfType ParenthesisBracket =
    BracketCharset { bracketTopLeft         = "⎛"
                   , bracketLeft            = "⎜"
                   , bracketBottomLeft      = "⎝"
                   , bracketTopRight        = "⎞"
                   , bracketRight           = "⎟"
                   , bracketBottomRight     = "⎠"
                   }

-- ─── CREATE BRACKET ─────────────────────────────────────────────────────────────

createBracketWithStyle :: BracketStlye -> SpacedBox -> SpacedBox
createBracketWithStyle style box = result where
    charset =
        parenthesisCharsOfType style
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
        [ bracketTopLeft    charset ] ++
        [ bracketLeft       charset | _ <- [ 3 .. bracketSize ] ] ++
        [ bracketBottomLeft charset ]
    rightBraceLines =
        [ bracketTopRight    charset ] ++
        [ bracketRight       charset | _ <- [ 3 .. bracketSize ] ] ++
        [ bracketBottomRight charset ]
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
