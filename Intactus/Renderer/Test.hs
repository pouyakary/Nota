
module Renderer.Test where
   
import Renderer.Text.Layout
import Renderer.Text.Shapes.Boxes



-- Test Box -----------------------------------------------------

createDemoShape :: String

createDemoShape = spacedBoxToString
                $ prependToEachLine "  "
                $ verticalConcat [left, right]
                
   where left = shapeBox Absolute
              $ shapeBox Bracket
              $ spacedBox "hello"
      
         right = shapeBox Floor
               $ shapeBox Absolute
               $ spacedBox "world"