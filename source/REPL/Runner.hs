
module REPL.Runner ( run ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Data.Set
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Types
import Language.FrontEnd.Parser
import Language.BackEnd.Renderer.Main
import Language.BackEnd.Evaluator.Main
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

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
    shapeBox LightBox $ spacedBox messageString where
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
        horizontalConcat [ outputSignSpacedbox, renderedOutputSpacedBox ]
    outputSignSpacedbox =
        spacedBox $ " In[" ++ number ++ "]:"
    renderedOutputSpacedBox =
        case parseIntactus input of
            Right ast   -> render    ast    False
            Left  error -> showError error

-- ────────────────────────────────────────────────────────────────────────────────
