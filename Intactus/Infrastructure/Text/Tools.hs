
module Infrastructure.Text.Tools where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Char
import Data.List.Split
import Data.List

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


-- ────────────────────────────────────────────────────────────────────────────────
