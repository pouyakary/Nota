
module Language.BackEnd.Renderer.Nodes.Identifier ( renderASTIdentifer ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.AST

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTIdentifer :: String -> SpacedBox
renderASTIdentifer x =
    spacedBox x

-- ────────────────────────────────────────────────────────────────────────────────
