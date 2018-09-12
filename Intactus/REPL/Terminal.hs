
module REPL.Terminal where    
   
import Model


-- Prompt ----------------------------------------

promptText :: Model -> String

promptText model = " In[" ++ number ++ "]: " where
      number = show $ promptNumber model


prompt :: Model -> IO String

prompt model = do putStr $ promptText model
                  input <- getLine
                  return input