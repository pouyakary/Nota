
module REPL.Runner where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Data.Scientific
import Data.Set
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Types
import Language.BackEnd.Evaluator.Main
import Language.BackEnd.Renderer.Main
import Language.FrontEnd.AST
import Language.FrontEnd.Parser
import Model
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

-- ─── TYPES ──────────────────────────────────────────────────────────────────────
                 --                    Rendered Scene
data RunnerResult = RunnerResult Model String

data RunnerMiddleData = RunnerMiddleData Model SpacedBox

-- ─── GETTERS ────────────────────────────────────────────────────────────────────

getRunnerResultModel :: RunnerResult -> Model
getRunnerResultModel ( RunnerResult x _ ) = x

getRunnerResultPrintable :: RunnerResult -> String
getRunnerResultPrintable ( RunnerResult _ x ) = x

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
            "ERROR: " ++ intercalate "\n       " uniqueMessages
        uniqueMessages =
            Data.Set.toList $ Data.Set.fromList nonEmptyMessages
        nonEmptyMessages =
            [ x | x <- stringedMessages, x /= "!" ]
        stringedMessages =
            [ showMessage x | x <- errorMessages error ]

-- ─── PRINT ERROR ────────────────────────────────────────────────────────────────

printParseError :: ParseError -> String -> String
printParseError error number =
    spacedBoxToString $ horizontalConcat [ promptSign, errorBox ] where
        promptSign =
            spacedBox $ " In[" ++ number ++ "]:"
        errorBox =
            showError error

-- ─── PRINT RESULT ───────────────────────────────────────────────────────────────

renderMath :: AST -> String -> SpacedBox
renderMath ast number =
    horizontalConcat [ outputSignSpacedbox, render ast False ] where
        outputSignSpacedbox =
            spacedBox $ " In[" ++ number ++ "]:"

-- ─── RENDER EVAL ERROR MESSAGE ──────────────────────────────────────────────────

renderEvalError :: String -> String -> SpacedBox
renderEvalError error promptNumber =
    shapeBox LightBox $ spacedBox error

-- ─── RENDER RESULT ──────────────────────────────────────────────────────────────

renderEvalResult :: [ Scientific ] -> SpacedBox
renderEvalResult results =
    spacedBox $ show $ results !! 0

-- ─── RUNNER ─────────────────────────────────────────────────────────────────────

run :: String -> Model -> RunnerResult
run input model = result where
    promptNumber =
        show $ length ( history model ) + 1
    result =
        -- Parsing
        case parseIntactus input of
            Left error ->
                RunnerResult model $ printParseError error promptNumber
            Right ast ->
                evalResult where
                    -- Evaluating
                    evalOutput =
                        case masterEval ast model of
                            Left error ->
                                RunnerMiddleData model $ renderEvalError error promptNumber
                            Right ( results, newModel ) ->
                                RunnerMiddleData newModel $ renderEvalResult results
                    evalResult =
                        rendereAndAppendNotation evalOutput
                    -- Rendering
                    notation =
                        renderMath ast promptNumber
                    -- Glue
                    rendereAndAppendNotation ( RunnerMiddleData outModel outPrint ) =
                        RunnerResult outModel printable where
                            printable =
                                spacedBoxToString $ verticalConcat boxes
                            boxes =
                                [ notation, spacedBox "", outputBox ]
                            outputBox =
                                horizontalConcat [ outputSign, outPrint ]
                            outputSign =
                                spacedBox $ "Out[" ++ promptNumber ++ "]:"

-- ────────────────────────────────────────────────────────────────────────────────
