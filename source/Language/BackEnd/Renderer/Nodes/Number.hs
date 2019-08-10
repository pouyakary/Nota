
module Language.BackEnd.Renderer.Nodes.Number ( renderASTNumber ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.AST
import Text.Regex
import Text.Printf
import Model

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTNumber :: Double -> SpacedBox
renderASTNumber x =
    spacedBox noZeroDecimalValue where
        noZeroDecimalValue =
            case matchRegex isNumberEndingWithDecimalsAndZeros numberString of
                Just x  -> subRegex decimalZeroRemover numberString ""
                Nothing -> numberString
        isNumberEndingWithDecimalsAndZeros =
            mkRegex "^([0-9]*)(\\.)([0-9]+)$"
        decimalZeroRemover =
            mkRegex "(\\.?)(0+)$"
        numberString =
            show x

-- ────────────────────────────────────────────────────────────────────────────────
