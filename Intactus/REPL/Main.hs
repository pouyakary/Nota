
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

runREPL model = do printTitle
                   repl model

-- ─── PRINT TITLE ────────────────────────────────────────────────────────────────

printTitle =
    do  windowWidth <- terminalWidth
        setTitle "✤ Kary Intactus Pro ✤"
        putStrLn $ spacedBoxToString $ createLogoForWidth windowWidth

        where
            createLogoForWidth w =
                if w == -1 then justLogoBox else fullBox w
            karyText =
                spacedBox "Kary"
            logoText =
                shapeBox Bracket $ spacedBox "Intactus Pro"
            logoBox =
                verticalConcat [ karyText, logoText ]
            justLogoBox =
                marginedBox ( BoxSize 1 0 0 1 ) logoBox
            fullBox w =
                result where
                    result =
                        verticalConcat [ leftLine, logoBox, rightLine ]
                    rightLineWidth =
                        5
                    leftLine =
                        spacedBox $ repeatText '─' ( w - ( width logoBox + 2 + rightLineWidth ) )
                    rightLine =
                        spacedBox $ repeatText '─' rightLineWidth


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
