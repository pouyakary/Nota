
module REPL.Text.Boxes where
   
import Data.List
import REPL.Text.Tools

-- Box Types ------------------------------------------

data BoxType =
   Bracket | Absolute | Floor | Ceiling
   

data BoxCharSet = BoxCharSet { boxTopLeft     :: Char
                             , boxTop         :: Char
                             , boxTopRight    :: Char
                             , boxRight       :: Char
                             , boxBottomRight :: Char
                             , boxBottom      :: Char
                             , boxBottomLeft  :: Char
                             , boxLeft        :: Char
                             }



-- Get Box Characters ---------------------------------

boxCharsOfType :: BoxType -> BoxCharSet

boxCharsOfType Bracket =
   BoxCharSet { boxTopLeft     = '┌'
              , boxTop         = ' '
              , boxTopRight    = '┐'
              , boxRight       = '│'
              , boxBottomRight = '┘'
              , boxBottom      = ' '
              , boxBottomLeft  = '└'
              , boxLeft        = '│'
              }
                 
boxCharsOfType Absolute =
   BoxCharSet { boxTopLeft     = '│'
              , boxTop         = ' '
              , boxTopRight    = '│'
              , boxRight       = '│'
              , boxBottomRight = '│'
              , boxBottom      = ' '
              , boxBottomLeft  = '│'
              , boxLeft        = '│'
              }
                 
boxCharsOfType Floor =
   BoxCharSet { boxTopLeft     = '│'
              , boxTop         = ' '
              , boxTopRight    = '│'
              , boxRight       = '│'
              , boxBottomRight = '┘'
              , boxBottom      = ' '
              , boxBottomLeft  = '└'
              , boxLeft        = '│'
              }
                 
boxCharsOfType Ceiling =
   BoxCharSet { boxTopLeft     = '┌'
              , boxTop         = ' '
              , boxTopRight    = '┐'
              , boxRight       = '│'
              , boxBottomRight = '│'
              , boxBottom      = ' '
              , boxBottomLeft  = '│'
              , boxLeft        = '│'
              }



-- Create Box -----------------------------------------

box boxType content =
   let
      text =
         spacedBox content
         
      size =
         lengthOfTheLongestLine text
         
      charSet =
         boxCharsOfType boxType
         
      firstLine =
         [boxTopLeft charSet] ++
         (repeatText (boxTop charSet) (size + 2)) ++
         [boxTopRight charSet]
      
      middleLines =
         [ [boxLeft charSet] ++ " " ++ line ++
           " " ++ [boxRight charSet]
           | line <- lines text ]
      
      lastLine =
         [boxBottomLeft charSet] ++
         (repeatText (boxBottom charSet) (size + 2)) ++
         [boxBottomRight charSet]
         

   in
      intercalate "\n"
         ([ firstLine ] ++ middleLines ++ [ lastLine ])
         





