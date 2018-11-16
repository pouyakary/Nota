
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
                       , baseLine = resultHeight `div` 2
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
        left =
            if boxWidth - ( width spacedText ) == 0
                then 0
                else ( boxWidth - width spacedText ) `div` 2
        right =
            boxWidth - ( left + width spacedText )
        bottom =
            boxHeight - ( top + height spacedText )


-- ─── VERTICAL CONCAT ────────────────────────────────────────────────────────────

verticalConcat :: [SpacedBox] -> SpacedBox
verticalConcat boxes = SpacedBox { boxLines = resultLines
                                 , width    = resultWidth
                                 , height   = resultHeight
                                 , baseLine = resultBaseLine
                                 }
    where
        resultHeight =
            maximum [ length ( boxLines x ) | x <- boxes ]
        centeredBoxlines =
            [ centerText ( width x ) resultHeight x | x <- boxes ]
        resultWidth =
            sum [ 1 + width x | x <- boxes ] - 1
        resultLines =
            [ intercalate " " [ ( boxLines x ) !! lineNumber | x <- centeredBoxlines ]
                | lineNumber <- [ 0.. ( resultHeight - 1 ) ] ]
        resultBaseLine =
            if odd resultHeight
                then resultHeight `div` 2
                else resultHeight `div` 2


-- ─── VERTICAL CONCAT WITHOUT INTERMEDIATE SPACE ─────────────────────────────────

verticalConcatWithoutSpace :: [SpacedBox] -> SpacedBox
verticalConcatWithoutSpace boxes =
    SpacedBox { boxLines = resultLines
              , width    = resultWidth
              , height   = resultHeight
              , baseLine = resultHeight `div` 2
              }
    where
        resultHeight =
            maximum [ length ( boxLines x ) | x <- boxes ]
        centeredBoxlines =
            [ centerText ( width x ) resultHeight x | x <- boxes ]
        resultWidth =
            sum [ width x | x <- boxes ]
        resultLines =
            [ intercalate "" [ ( boxLines x ) !! lineNumber | x <- centeredBoxlines ]
                | lineNumber <- [ 0.. ( resultHeight - 1 ) ] ]

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


-- ─── BASELINE CENTER ────────────────────────────────────────────────────────────

baselineCentered :: SpacedBox -> SpacedBox
baselineCentered box = result
    where
        boxCenter =
            if even $ height box
                then height box `div` 2 - 1
                else height box `div` 2
        marginSize =
            abs $ ( height box ) - ( 2 * baseLine box )  - 1
        marginSetting =
            if baseLine box > boxCenter
                then BoxSize 0 0 marginSize 0
                else BoxSize marginSize 0 0 0
        result =
            if baseLine box == boxCenter
                then box
                else marginedBox marginSetting box

-- ─── BASELINE VERTICAL CONTACT ──────────────────────────────────────────────────

baselineVerticalConcat :: [SpacedBox] -> SpacedBox
baselineVerticalConcat boxes =
    verticalConcat [ baselineCentered x | x <- boxes ]

-- ────────────────────────────────────────────────────────────────────────────────
