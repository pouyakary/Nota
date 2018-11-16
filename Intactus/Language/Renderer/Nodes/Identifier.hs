
module Language.Renderer.Nodes.Identifier ( renderASTIdentifer ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTIdentifer :: String -> SpacedBox
renderASTIdentifer x =
    spacedBox x

-- ────────────────────────────────────────────────────────────────────────────────
