
module REPL.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Model
import REPL.Terminal
import REPL.Runner
import Infrastructure.Text.Layout
import Infrastructure.Text.Shapes.Boxes
import Language.FrontEnd.Parser

-- ─── RUN REPL ───────────────────────────────────────────────────────────────────

runREPL model = do printTitle
                   repl model

-- ─── PRINT TITLE ────────────────────────────────────────────────────────────────

printTitle =
    do  putStrLn ""
        putStrLn $ spacedBoxToString logoBox
        putStrLn ""

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
    do  input <- prompt number
        putStrLn $ run input number
        putStrLn "\n"
        repl $ updateModel model input
        where
            number = show $ length ( history model ) + 1

-- ─── UPDATE MODEL ───────────────────────────────────────────────────────────────

updateModel model input =
    model { history = ( history model ) ++ [ input ]
          }

-- ────────────────────────────────────────────────────────────────────────────────
