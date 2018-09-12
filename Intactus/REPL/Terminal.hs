
module REPL.Terminal where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model

-- ─── PROMPT ─────────────────────────────────────────────────────────────────────

promptText :: Model -> String
promptText model = " In[" ++ number ++ "]: " where
      number = show $ promptNumber model

prompt :: Model -> IO String
prompt model = do putStr $ promptText model
                  input <- getLine
                  return input