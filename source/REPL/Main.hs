
module REPL.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Data.Scientific
import Data.Set
import Debug.Trace
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Types
import Infrastructure.Text.Tools
import Language.BackEnd.Evaluator.Main
import Language.BackEnd.Renderer.Main
import Language.FrontEnd.AST
import Language.FrontEnd.Parser
import Model
import Prelude hiding (catch)
import REPL.Terminal
import System.Console.ANSI
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error


-- ─── TYPES ──────────────────────────────────────────────────────────────────────

data RunnerResult =
    RunnerResult Model String

data RunnerMiddleData =
    RunnerMiddleData Model SpacedBox


-- ─── GETTERS ────────────────────────────────────────────────────────────────────

getRunnerResultModel :: RunnerResult -> Model
getRunnerResultModel ( RunnerResult x _ ) = x

getRunnerResultPrintable :: RunnerResult -> String
getRunnerResultPrintable ( RunnerResult _ x ) = x


-- ─── RUN REPL ───────────────────────────────────────────────────────────────────

runREPL model =
    do  printTitle
        repl model

repl model =
    do  putStrLn ""
        input    <- prompt $ show $ length ( history model ) + 1
        newModel <- pure $ run input model
        repl newModel


-- ─── PRINT TITLE ────────────────────────────────────────────────────────────────

printTitle :: String
printTitle =
    do  windowWidth <- terminalWidth
        setTitle "✤ Kary Nota ✤"
        putStrLn $ createLogoForWidth windowWidth

        where
            name = "Kary Nota"

            createLogoForWidth :: Int -> String
            createLogoForWidth w =
                if w == -1 then name else fullBox w

            fullBox :: Int -> String
            fullBox w =
                result where
                    result =
                        leftLine ++ " " ++ name ++ " " ++ rightLine
                    rightLineWidth =
                        5
                    leftLine =
                        repeatText '─' $ w - ( length name + 2 + rightLineWidth )
                    rightLine =
                        repeatText '─' rightLineWidth


-- ─── SHEW ERROR MESSAGES ────────────────────────────────────────────────────────

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

        showMessage :: Message -> String
        showMessage message =
            case message of SysUnExpect x -> wrapp x "Unexpected "
                            UnExpect    x -> wrapp x "Unexpected "
                            Expect      x -> wrapp x "Expected "
                            Message     x -> wrapp x ""
            where
                wrapp x prefix =
                    if x /= "" then prefix ++ x else "!"


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

run :: String -> Model -> Model
run input model = catch result handler where
    handler ex =
        do  putStrLn $ printParseError ( show ex ) promptNumber
            return model
    result =
        do  ast                    <-  parseIntactus input
            ( results, newModel )  <-  pure $ masterEval ast model
            notation               <-  pure $ renderMath ast promptNumber
            evalResult             <-  pure $ rendereAndAppendNotation notation evalOutput
            renederedEvalResult    <-  pure $ renderEvalResult results
            putStrLn renderEvalResult
            return newModel

    promptNumber =
        show $ length ( history model ) + 1
    rendereAndAppendNotation notation ( RunnerMiddleData outModel outPrint ) =
        RunnerResult outModel printable where
            printable =
                spacedBoxToString $ verticalConcat boxes
            boxes =
                [ notation, spacedBox "", outputBox ]
            outputBox =
                horizontalConcat [ outputSign, outPrint ]
            outputSign =
                spacedBox $ "Out[" ++ promptNumber ++ "]:"

-- run :: String -> Model -> RunnerResult
-- run input model = result where
--     promptNumber =
--         show $ length ( history model ) + 1
--     result =
--         -- Parsing
--         case parseIntactus input of
--             Left error ->
--                 RunnerResult model $ printParseError error promptNumber
--             Right ast ->
--                 evalResult where
--                     -- Evaluating
--                     evalOutput =
--                         case masterEval ast model of
--                             Left error ->
--                                 RunnerMiddleData model $ renderEvalError error promptNumber
--                             Right ( results, newModel ) ->
--                                 RunnerMiddleData newModel $ renderEvalResult results
--                     evalResult =
--                         rendereAndAppendNotation evalOutput
--                     -- Rendering


-- ────────────────────────────────────────────────────────────────────────────────
