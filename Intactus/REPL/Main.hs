
module REPL.Main where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import REPL.Terminal
import Model
import Renderer.Test
import Renderer.Text.Layout
import Renderer.Text.Shapes.Boxes
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

repl model = do input <- prompt model
                repl $ updateModel model input

-- ─── UPDATE MODEL ───────────────────────────────────────────────────────────────

updateModel model input =
    model { promptNumber = ( promptNumber model ) + 1
          , history      = ( history model ) ++ [ input ]
          }

-- ────────────────────────────────────────────────────────────────────────────────
