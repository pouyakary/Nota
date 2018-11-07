
module Language.FrontEnd.Parser ( parseIntactus ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Text.ParserCombinators.Parsec
import Data.Scientific
import Data.List.Split ( splitOn )


-- ─── VALUES ─────────────────────────────────────────────────────────────────────

intValues :: GenParser Char st AST
intValues =
    try intFunctionCall <|> intNumber <|> intIdentifier


-- ─── PARES IDENTIFIER ───────────────────────────────────────────────────────────

intIdentifierTailPart :: GenParser Char st String
intIdentifierTailPart = do
    parts <- many1 alphaNum
    return parts

intIdentifierSpacedPart :: GenParser Char st String
intIdentifierSpacedPart = do
    spaceParts <- many1 ( space <|> tab )
    letterPart <- letter
    return ( " " ++ [ letterPart ] )

intIdentifierLetterr :: GenParser Char st String
intIdentifierLetterr = do
    letterPart <- letter
    return [ letterPart ]

intIdentifier :: GenParser Char st AST
intIdentifier = do
    name <- many1 ( intIdentifierLetterr <|> try intIdentifierSpacedPart )
    return ( ASTIdentifer ( intercalate "" name ) )


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
    integerPart  <- many1 digit <?> "numeric base part"
    decimalPart  <- intNumberDecimalPart <|> return "" <?> "numeric decimal part"
    exponentPart <- intExponentialPart <|> return "" <?> "numeric exponent part"
    return ( ASTNumber ( read
        ( integerPart ++ decimalPart ++ exponentPart ) :: Scientific ) )


-- ─── FUNCTION CALL ──────────────────────────────────────────────────────────────

intFunctionArgsSeparator :: GenParser Char st ( )
intFunctionArgsSeparator = do
    spaces
    char ','
    spaces
    return ( )

intFunctionArgs :: GenParser Char st [ AST ]
intFunctionArgs = do
    args <- intValues `sepBy` ( try intFunctionArgsSeparator )
    return args

intFunctionCall :: GenParser Char st AST
intFunctionCall = do
    functionName <- intIdentifier
    char '['
    spaces
    args <- ( try intFunctionArgs ) <|> return [ ]
    spaces
    char ']'
    return ( ASTFunctionCall functionName args )


-- ─── PARSER ─────────────────────────────────────────────────────────────────────

parseIntactus :: String -> Either ParseError AST
parseIntactus input =
    parse parseNumber "" input


-- ────────────────────────────────────────────────────────────────────────────────
