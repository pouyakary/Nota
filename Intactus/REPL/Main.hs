
module REPL.Main where 

import REPL.Terminal    
import Model
import Renderer.Test
import Renderer.Text.Layout
import Renderer.Text.Shapes.Boxes



-- Run REPL ------------------------------------------------

runREPL model = do printTitle
                   repl model

-- Print Title ---------------------------------------------

printTitle =
   do putStrLn ""
      putStrLn (spacedBoxToString logoBox)
      putStrLn ""
      
      where
         karyText =
            centerText 4 3 $ spacedBox "KARY"
         versionText =
            centerText 5 3 $ spacedBox ": I I"
         logoText =
            shapeBox Bracket $ spacedBox "I N T A C T U S"

         logoBox =
            prependToEachLine " "
               $ verticalConcat
                 [karyText, logoText, versionText]
                  


-- REPL Body -----------------------------------------------

repl model = do input <- prompt model
                repl $ updateModel model input



-- Update Model ---------------------------------------------

updateModel model input = 
   model { promptNumber = (promptNumber model) + 1
         , history      = (history model) ++ [input]
         }




