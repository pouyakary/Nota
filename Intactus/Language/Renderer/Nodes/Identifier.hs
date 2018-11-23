
module Language.Renderer.Nodes.Identifier ( renderASTIdentifer ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.Types

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTIdentifer :: String -> SpacedBox
renderASTIdentifer x =
    spacedBox x

-- ────────────────────────────────────────────────────────────────────────────────
