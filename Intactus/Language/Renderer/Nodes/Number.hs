
module Language.Renderer.Nodes.Number ( renderASTNumber ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout
import Data.Scientific
import Text.Regex

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTNumber :: Scientific -> SpacedBox
renderASTNumber x =
    spacedBox noZeroDecimalValue where
        noZeroDecimalValue =
            subRegex ( mkRegex ".0$" ) value ""
        value =
            if length stringedNumber > 10
                then formatScientific Generic ( Just 10 ) x
                else stringedNumber
        stringedNumber =
            show x

-- ────────────────────────────────────────────────────────────────────────────────
