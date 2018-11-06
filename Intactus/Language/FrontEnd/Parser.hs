
module Language.FrontEnd.Parser ( parseIntactus ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Text.ParserCombinators.Parsec
import Data.Scientific
import Data.List.Split ( splitOn )

-- ─── PARES IDENTIFIER ───────────────────────────────────────────────────────────

intIdentifier :: GenParser Char st ASTIdentifer
intIdentifier =
    do  nameParts  <- ( many1 alphaNum ) `sepBy` spaces
        return ( ASTIdentifer nameParts )

-- ─── PARSE NUMBER ───────────────────────────────────────────────────────────────

intNumberDecimalPart :: GenParser Char st String
intNumberDecimalPart = do
    char '.'
    decimals <- many1 digit
    return $ "." ++ decimals

intExponentialPart :: GenParser Char st String
intExponentialPart = do
    oneOf "eE"
    exponentialPart <- many1 digit
    return $ "e" ++ exponentialPart

intNumber :: GenParser Char st ASTNumber
intNumber = do
    integerPart     <- many1 digit
    decimalPart     <- intNumberDecimalPart <|> return ""
    exponentialPart <- intExponentialPart <|> return ""
    return ( ASTNumber ( read
        ( integerPart ++ decimalPart ++ exponentialPart ) :: Scientific ) )

-- ─── PARSER ─────────────────────────────────────────────────────────────────────

parseIntactus :: String -> Either ParseError AST
parseIntactus input =
    parse parseNumber "" input

-- ────────────────────────────────────────────────────────────────────────────────
