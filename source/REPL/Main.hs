
module REPL.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import REPL.Terminal
import Control.Exception
import Data.List
import Data.List
import Data.Set
import Debug.Trace
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Brackets
import Infrastructure.Text.Shapes.Presets
import Infrastructure.Text.Shapes.Types
import Infrastructure.Text.Tools
import Language.BackEnd.Evaluator.Main
import Language.BackEnd.Evaluator.Types
import Language.BackEnd.Renderer.Main
import Language.BackEnd.Renderer.Nodes.Number
import Language.FrontEnd.AST
import Language.FrontEnd.Parser
import Model
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

runREPL :: Model -> IO ( )
runREPL model =
    do  printTitle
        repl  model
        where
            repl :: Model -> IO ( )
            repl model =
                do  input    <- prompt $ show $ length ( history model ) + 1
                    newModel <- run input model
                    putStrLn ""
                    repl newModel

-- ─── PRINT TITLE ────────────────────────────────────────────────────────────────

printTitle =
    do  windowWidth <- terminalWidth
        setTitle "Kary Nota"
        putStrLn $ createLogoForWidth windowWidth
        putStrLn ""

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
            spacedBox $ "  In[!]:"
        errorBox =
            showError error

-- ─── PRINT RESULT ───────────────────────────────────────────────────────────────

renderMath :: AST -> String -> SpacedBox
renderMath ast number =
    horizontalConcat [ outputSignSpacedbox, render ast False ] where
        outputSignSpacedbox =
            spacedBox $ "  In[" ++ number ++ "]:"

-- ─── RENDER EVAL ERROR MESSAGE ──────────────────────────────────────────────────

renderEvalError :: String -> SpacedBox
renderEvalError error =
    shapeBox LightBox $ spacedBox ( "ERROR: " ++ error )

-- ─── RENDER RESULT ──────────────────────────────────────────────────────────────

renderEvalResult :: [ Double ] -> SpacedBox
renderEvalResult results =
    case length results of
        1 ->
            spacedBox $ show (head results)
        _ ->
            createBracketWithStyle Bracket boxedArgs where
                comma =
                    spacedBox ":"
                boxedArgs =
                    horizontalConcat $ argsHead ++ argsTail
                argsHead =
                    [ spacedBox $ show $ head results ]
                argsTail =
                    concat [ [ comma, spacedBox $ show x ] | x <- tail results ]

-- ─── RUNNER ─────────────────────────────────────────────────────────────────────

run :: String -> Model -> IO Model
run input model =
    case parseIntactus input of
        Left  err ->
            do  putStrLn $ printParseError err promptNumber
                return model
        Right ast ->
            do  notation  <-  pure $ renderMath ast promptNumber
                putStrLn $ spacedBoxToString notation
                putStrLn ""
                newModel  <-  runEval ast model input promptNumber
                putStrLn ""
                return newModel
    where
        promptNumber =
            show $ length ( history model ) + 1

renderOutput :: SpacedBox -> String -> IO ( )
renderOutput message promptNumber =
    do  putStrLn output
    where
        output =
            spacedBoxToString $ horizontalConcat [ outputSign, message ]
        outputSign =
            spacedBox $ " Out[" ++ promptNumber ++ "]:"

runEval :: AST -> Model -> String -> String -> IO Model
runEval ast model inputString promptNumber =
    case masterEval ast model inputString of
        Left err ->
            do  renderOutput ( renderEvalError err ) promptNumber
                return model
        Right ( MasterEvalResultRight results newModel ) ->
            do  renderOutput ( renderEvalResult results ) promptNumber
                return newModel

-- ────────────────────────────────────────────────────────────────────────────────
