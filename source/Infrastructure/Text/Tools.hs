
module Infrastructure.Text.Tools where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Char
import Data.List
import Data.List.Split

-- ─── REPEAT ─────────────────────────────────────────────────────────────────────

repeatText :: Char -> Int -> String
repeatText char times =
    [ char | _ <- [ 1..times ] ]

-- ─── REMOVE FROM START ──────────────────────────────────────────────────────────

removeFromStartOf :: String -> String -> Maybe String
removeFromStartOf removable text
    | removable `isPrefixOf` text =
        let
            start = length removable
            end   = length text - 1
        in
            Just [ text !! x | x <- [ start..end ] ]
    | otherwise =
       Nothing

-- ─── LENGTH OF THE LOGEST LINE ──────────────────────────────────────────────────

lengthOfTheLongestLine :: String -> Int
lengthOfTheLongestLine text =
    maximum [ length x | x <- lines text ]

-- ─── LINE IS ALL WHITE SPACES ───────────────────────────────────────────────────

lineIsAllSpaceChars :: String -> Bool
lineIsAllSpaceChars [ ] =
    True
lineIsAllSpaceChars ( x : xs ) =
    if x == ' '
        then lineIsAllSpaceChars xs
        else False

-- ─── FIT INTO COLUMN ────────────────────────────────────────────────────────────

-- fitIntoColumn :: Int -> SpacedBox -> SpacedBox
-- fitIntoColumn expectedWidth textBox = result where
--     result =
--         SpacedBox { boxLines = resultLines
--                   , width    = resultWidth
--                   , height   = boxHeight
--                   , baseLine = boxBaseLine
--                   }
--     boxBaseLine =
--         baseLine textBox
--     boxHeight =
--         (ceiling (( width textBox ) / expectedWidth)) * (height textbox)

--     generateLinesWithLinesOf lines withOfInput =
--         startingLines =
--             if widthOfInput > expectedWidth
--                 case ( splitAt expectedWidth lines) of
--                     ()

--                 else resultLessor where


-- ────────────────────────────────────────────────────────────────────────────────
