
module REPL.Terminal where    

import Model


-- Prompt ----------------------------------------

promptText model =
   let
      number = (show (promptNumber model))
   in
      " In[" ++ number ++ "]: "


prompt model = do putStr (promptText model)
                  input <- getLine
                  return input
