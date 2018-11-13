
module Language.Renderer.Nodes.Number ( rendereASTNumber ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Data.Scientific

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

rendereASTNumber :: Scientific -> String
rendereASTNumber x =
    let stringedNumber = show x
    in  if length stringedNumber > 10
            then formatScientific Generic (Just 10) x
            else stringedNumber

-- ────────────────────────────────────────────────────────────────────────────────
