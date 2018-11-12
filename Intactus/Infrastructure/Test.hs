
module Infrastructure.Test where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes

-- ─── TESTING BOX ────────────────────────────────────────────────────────────────

createDemoShape :: String
createDemoShape = spacedBoxToString
                $ prependToEachLine "  "
                $ verticalConcat [ left, right ]

   where left = shapeBox Absolute
              $ shapeBox Bracket
              $ spacedBox "hello"

         right = shapeBox Floor
               $ shapeBox Absolute
               $ spacedBox "world"

-- ────────────────────────────────────────────────────────────────────────────────
