
module Infrastructure.Text.Layout where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.List
import Infrastructure.Text.Tools

-- ─── TYPES ──────────────────────────────────────────────────────────────────────

data SpacedBox = SpacedBox { boxLines :: [String]
                           , width    :: Int
                           , height   :: Int
                           , baseLine :: Int
                           }

               -------  Top  Right  Bottom  Left
data BoxSize = BoxSize  Int  Int    Int     Int

data BoxSide = TopSideOf | RightSideOf | BottomSideOf | LeftSideOf


-- ─── GETTING BOX SIZE OF ────────────────────────────────────────────────────────

boxSizeOf :: BoxSide -> BoxSize -> Int
boxSizeOf TopSideOf    ( BoxSize x _ _ _ ) = x
boxSizeOf RightSideOf  ( BoxSize _ x _ _ ) = x
boxSizeOf BottomSideOf ( BoxSize _ _ x _ ) = x
boxSizeOf LeftSideOf   ( BoxSize _ _ _ x ) = x


-- ─── SPACED LINE ────────────────────────────────────────────────────────────────

spacedLineWithSize :: String -> Int -> String
spacedLineWithSize text size =
    text ++ tail where
        tail = repeatText ' ' $ size - length text


-- ─── CREAT TEXT BOX ─────────────────────────────────────────────────────────────

spacedBox :: String -> SpacedBox
spacedBox text = SpacedBox { boxLines = linesOfText
                           , width    = size
                           , height   = boxHeight
                           , baseLine = boxHeight `div` 2
                           }
    where
        boxHeight =
            length linesOfText
        size =
            lengthOfTheLongestLine text
        linesOfText =
            [ spacedLineWithSize x size | x <- lines text ]


-- ─── LINES TEXT ─────────────────────────────────────────────────────────────────

spacedBoxToString :: SpacedBox -> String
spacedBoxToString box =
    intercalate "\n" $ boxLines box


-- ─── MARGINED SPACE BOX ─────────────────────────────────────────────────────────

marginedBox :: BoxSize -> SpacedBox -> SpacedBox
marginedBox marginSettings text = result where
    top =
        boxSizeOf TopSideOf marginSettings
    right =
        boxSizeOf RightSideOf marginSettings
    bottom =
        boxSizeOf BottomSideOf marginSettings
    left =
        boxSizeOf LeftSideOf marginSettings
    paddingLine =
        repeatText ' ' $ left + ( width text ) + right
    topPadding =
        [ paddingLine | _ <- [ 1..top ] ]
    rightPadding =
        repeatText ' ' $ boxSizeOf RightSideOf marginSettings
    bottomPadding =
        [ paddingLine | _ <- [ 1..bottom ] ]
    leftPadding =
        repeatText ' ' $ boxSizeOf LeftSideOf marginSettings
    spacedBase =
        prependToEachLine rightPadding $ appendToEachLine leftPadding text
    resultLines =
        topPadding ++ boxLines spacedBase ++ bottomPadding
    resultWidth =
        left + right + width text
    resultHeight =
        top + bottom + height text

    result = SpacedBox { boxLines = resultLines
                       , width    = resultWidth
                       , height   = resultHeight
                       , baseLine = top + baseLine text
                       }


-- ─── MARGINED SPACE BOX ─────────────────────────────────────────────────────────

centerText :: Int -> Int -> SpacedBox -> SpacedBox
centerText boxWidth boxHeight spacedText =
    marginedBox boxSize spacedText where
        boxSize =
            BoxSize top right bottom left
        top =
            if boxHeight - ( height spacedText ) == 0
                then 0
                else ( boxHeight - height spacedText ) `div` 2
        right =
            if boxWidth - ( width spacedText ) == 0
                then 0
                else ( boxWidth - width spacedText ) `div` 2
        left =
            boxWidth - ( right + width spacedText )
        bottom =
            boxHeight - ( top + height spacedText )

-- ─── APPEND TO ALL LINES ────────────────────────────────────────────────────────

appendToEachLine :: String -> SpacedBox -> SpacedBox
appendToEachLine appendable base =
    SpacedBox { boxLines = result
              , width    = length appendable + width base
              , height   = height base
              , baseLine = baseLine base
              }
    where
        result = [ line ++ appendable | line <- boxLines base ]

-- ─── PREPEND TO ALL LINES ───────────────────────────────────────────────────────

prependToEachLine :: String -> SpacedBox -> SpacedBox
prependToEachLine prependable base =
    SpacedBox { boxLines = result
              , width    = length prependable + width base
              , height   = height base
              , baseLine = baseLine base
              }
    where
        result = [ prependable ++ line | line <- boxLines base ]

-- ─── VERTICAL CONCAT WITHOUT INTERMEDIATE SPACE ─────────────────────────────────

verticalConcatWithoutSpace :: [ SpacedBox ] -> SpacedBox
verticalConcatWithoutSpace =
    verticalConcatCore ""

-- ─── VERTICAL CONCAT ────────────────────────────────────────────────────────────

verticalConcat :: [ SpacedBox ] -> SpacedBox
verticalConcat =
    verticalConcatCore " "

-- ─── VERTICAL CONCAT CORE ───────────────────────────────────────────────────────

verticalConcatCore :: String -> [ SpacedBox ] -> SpacedBox
verticalConcatCore spacing boxes = result where
    spacingWidth =
        length spacing
    maxBaseline =
        maximum [ baseLine x | x <- boxes ]
    maxBaselineToHeightDifference =
        ( -1 ) + maximum [ height x - baseLine x | x <- boxes ]
    resultHeight =
        maxBaseline + 1 + maxBaselineToHeightDifference
    resultWidth =
        sum [ spacingWidth + width x | x <- boxes ] - spacingWidth
    spacedBoxes =
        [ spaceSingeBox x | x <- boxes ]
    spaceSingeBox box =
        marginedBox marginSettings box where
            differenceToTop =
                maxBaseline - baseLine box
            differenceToBottom =
                resultHeight - differenceToTop - height box
            marginSettings =
                BoxSize differenceToTop 0 differenceToBottom 0
    resultLines =
        [ intercalate spacing [ ( boxLines x ) !! lineNumber | x <- spacedBoxes ]
            | lineNumber <- [ 0 .. resultHeight - 1 ] ]
    result = SpacedBox { boxLines = resultLines
                       , width    = resultWidth
                       , height   = resultHeight
                       , baseLine = maxBaseline
                       }

-- ────────────────────────────────────────────────────────────────────────────────
