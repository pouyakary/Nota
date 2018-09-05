
module REPL.Main where 

import REPL.Terminal    
import Model
import REPL.Text.Boxes


-- Run REPL --------------------------------------

runREPL model = do printTitle
                   repl model


-- Print Title -----------------------------------

replHeaderText =
   " ┌──────┬────────────────────────────────────────────┐\n\              
   \ │ KARY │  I N T A C T U S   L A N G U A G E   I I   │\n\              
   \ └──────┴────────────────────────────────────────────┘"                
                                  

testPrint =
   do putStrLn (box Bracket "hello\n----------------")
                                         
printTitle =
   do putStrLn ""
      putStrLn replHeaderText
      putStrLn ""
      testPrint
      


-- REPL Body -------------------------------------

repl model = do input <- prompt model
                repl (updateModel model input)
                


-- Update Model ----------------------------------

updateModel model input = 
   model { promptNumber = (promptNumber model) + 1
         , history = (history model) ++ [input]
         }