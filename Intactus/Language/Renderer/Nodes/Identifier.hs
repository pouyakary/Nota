
module Language.Renderer.Nodes.Identifier ( rendereASTIdentifer ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Infrastructure.Text.Layout

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

rendereASTIdentifer :: String -> SpacedBox
rendereASTIdentifer x =
    spacedBox x

-- ────────────────────────────────────────────────────────────────────────────────
