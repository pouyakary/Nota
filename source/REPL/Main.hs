
module REPL.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Infrastructure.Text.Tools
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Types
import Language.FrontEnd.Parser
import Model
import REPL.Runner
import REPL.Terminal
import System.Console.ANSI

-- ─── RUN REPL ───────────────────────────────────────────────────────────────────

runREPL model =
    do  printTitle
        repl model
        where
            repl model =
                do  putStrLn ""
                    input <- prompt $ show $ length ( history model ) + 1
                    results <- pure $ run input model
                    putStrLn $ getRunnerResultPrintable results
                    repl $ getRunnerResultModel results

-- ─── PRINT TITLE ────────────────────────────────────────────────────────────────

printTitle =
    do  windowWidth <- terminalWidth
        setTitle "✤ Kary Nota ✤"
        putStrLn $ createLogoForWidth windowWidth

        where
            createLogoForWidth w =
                if w == -1 then name else fullBox w
            name =
                "Kary Nota"
            fullBox w =
                result where
                    result =
                        leftLine ++ " " ++ name ++ " " ++ rightLine
                    rightLineWidth =
                        5
                    leftLine =
                        repeatText '─' ( w - ( length name + 2 + rightLineWidth ) )
                    rightLine =
                        repeatText '─' rightLineWidth

-- ────────────────────────────────────────────────────────────────────────────────
