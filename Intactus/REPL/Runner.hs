
module REPL.Runner ( run ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Parser
import Language.Renderer.Main
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.List
import Data.Set

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

showError :: ParseError -> SpacedBox
showError error =
    shapeBox Corners $ spacedBox messageString where
        messageString =
            "ERROR: " ++ ( intercalate "\n       " uniqueMessages )
        uniqueMessages =
            Data.Set.toList $ Data.Set.fromList nonEmptyMessages
        nonEmptyMessages =
            [ x | x <- stringedMessages, x /= "!" ]
        stringedMessages =
            [ showMessage x | x <- errorMessages error ]

-- ─── RUNNER ─────────────────────────────────────────────────────────────────────

run :: String -> String -> String
run input number = output where
    output =
        spacedBoxToString outputSpacedBox
    outputSpacedBox =
        trimWhiteSpaceLines $
            baselineVerticalConcat [ outputSignSpacedbox, renderedOutputSpacedBox ]
    outputSignSpacedbox =
        spacedBox $ " In[" ++ number ++ "]:"
    renderedOutputSpacedBox =
        case parseIntactus input of
            Right ast   -> render    ast
            Left  error -> showError error

-- ────────────────────────────────────────────────────────────────────────────────
