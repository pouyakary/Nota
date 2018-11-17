
module REPL.Terminal where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model
import System.Console.ANSI
import System.IO

-- ─── PROMPT ─────────────────────────────────────────────────────────────────────

prompt :: String -> IO String
prompt number = do
    putStr $ " In[" ++ number ++ "]: "
    hFlush stdout
    input <- getLine
    cursorUpLine 1
    clearLine
    return input

-- ────────────────────────────────────────────────────────────────────────────────
