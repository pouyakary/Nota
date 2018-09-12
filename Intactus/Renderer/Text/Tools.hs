 
module Renderer.Text.Tools where

import Data.Char
import Data.List.Split
import Data.List



-- Repeat -------------------------------------------------------

repeatText :: Char -> Int -> String

repeatText char times = [ char | _ <- [ 1..times ] ]



-- Remove From Start --------------------------------------------

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



-- Length Of The Longest Line -----------------------------------

lengthOfTheLongestLine :: String -> Int

lengthOfTheLongestLine text =
   maximum [ length x | x <- lines text ]





