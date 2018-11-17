
module REPL.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Infrastructure.Text.Shapes.Types
import Language.FrontEnd.Parser
import Model
import REPL.Runner
import REPL.Terminal
import System.Console.ANSI

-- ─── RUN REPL ───────────────────────────────────────────────────────────────────

runREPL model = do printTitle
                   repl model

-- ─── PRINT TITLE ────────────────────────────────────────────────────────────────

printTitle =
    do  setTitle "✤ Kary Intactus Pro ✤"
        putStrLn ""
        putStrLn $ spacedBoxToString logoBox

        where
            karyText =
                centerText 7 3 $ spacedBox "K A R Y"
            versionText =
                centerText 5 3 $ spacedBox "P R O"
            logoText =
                shapeBox Bracket $ spacedBox "I N T A C T U S"
            logoBox =
                prependToEachLine " "
                    $ verticalConcat [ karyText, logoText, versionText ]

-- ─── REPL BODY ──────────────────────────────────────────────────────────────────

repl model =
    do  putStrLn ""
        input <- prompt number
        putStrLn $ run input number
        repl $ updateModel model input
        where
            number = show $ length ( history model ) + 1

-- ─── UPDATE MODEL ───────────────────────────────────────────────────────────────

updateModel model input =
    model { history = ( history model ) ++ [ input ]
          }

-- ────────────────────────────────────────────────────────────────────────────────
