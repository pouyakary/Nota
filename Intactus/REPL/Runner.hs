
module REPL.Runner ( run ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Parser
import Language.Renderer.Main
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.List

-- ─── SHEW ERROR MESSAGES ────────────────────────────────────────────────────────

showMessage :: Message -> String
showMessage message =
    case message of SysUnExpect x -> wrapp x "Unexpected "
                    UnExpect    x -> wrapp x "Unexpected "
                    Expect      x -> wrapp x "Expected "
                    Message     x -> wrapp x ""
    where
        wrapp x prefix =
            if x /= "" then prefix ++ x else "!"

showError :: ParseError -> String
showError error =
    intercalate ", " nonEmptyMessages where
        nonEmptyMessages =
            [ x | x <- stringedMessages, x /= "!" ]
        stringedMessages =
            [ showMessage x | x <- errorMessages error ]

-- ─── RUNNER ─────────────────────────────────────────────────────────────────────

run :: String -> String
run input =
    case parseIntactus input of
        Right ast   -> render    ast
        Left  error -> showError error

-- ────────────────────────────────────────────────────────────────────────────────
