
module Language.FrontEnd.Parser ( parseIntactus ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Language.FrontEnd.Types
import Control.Monad ((>>))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Scientific
import Data.List.Split ( splitOn )
import Data.List

-- ─── LEXER ──────────────────────────────────────────────────────────────────────

languageDef =
  emptyDef { Token.commentStart     = "(*"
           , Token.commentEnd       = "*)"
           , Token.commentLine      = "--"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedNames    = [ ]
           , Token.reservedOpNames  = [ "+", "-", "*", "/", "="
                                      , "<", ">", "^", "%" ]
           }

lexer           = Token.makeTokenParser languageDef
reservedOp      = Token.reservedOp lexer
haskellNumber   = Token.naturalOrFloat lexer


-- ─── NUMBER ─────────────────────────────────────────────────────────────────────

intNumber :: GenParser Char st AST
intNumber = do
    value <- haskellNumber
    spaces
    return $ ASTNumber ( case value of Right x -> toScientific x
                                       Left  x -> toScientific x )
    where
        toScientific x =
            read ( show x ) :: Scientific


-- ─── IDENTIFIER ─────────────────────────────────────────────────────────────────

intIdentifierTailPart :: GenParser Char st String
intIdentifierTailPart = do
    parts <- many1 alphaNum
    return parts

intIdentifierSpacedPart :: GenParser Char st String
intIdentifierSpacedPart = do
    spaceParts <- many1 ( space <|> tab )
    letterPart <- intIdentifierLetterr
    return $ ' ' : letterPart

intIdentifierLetterr :: GenParser Char st String
intIdentifierLetterr = do
    letterPart <- letter <|> digit <|> char '\''
    return [ letterPart ]

intIdentifierJoiner :: Char -> [String] -> [String]
intIdentifierJoiner start [ ] =
    [ [ start ] ]
intIdentifierJoiner start ( x : xs ) =
    [ start : x ] ++ xs

intIdentifier :: GenParser Char st AST
intIdentifier = do
    firstChar <- letter
    name <- many ( intIdentifierLetterr <|> try intIdentifierSpacedPart )
    spaces
    return $ ASTIdentifer ( intercalate ""
        ( intIdentifierJoiner firstChar name ) )


-- ─── VALUES ─────────────────────────────────────────────────────────────────────

intValues :: GenParser Char st AST
intValues =
    try intFunctionCall <|> intNumber <|> intIdentifier <?> "value"


-- ─── FACTOR ─────────────────────────────────────────────────────────────────────

intFactorWithParenthesis :: GenParser Char st AST
intFactorWithParenthesis = do
    char '('
    spaces
    value <- intExpresson
    char ')' <?> "end of parenthesis"
    spaces
    return $ ASTParenthesis value

intFactor :: GenParser Char st AST
intFactor =
    intFactorWithParenthesis <|> intValues <?> "factored expression"


-- ─── FUNCTION CALL ──────────────────────────────────────────────────────────────

intFunctionArgsSeparator :: GenParser Char st ( )
intFunctionArgsSeparator = do
    char ',' <?> "argument separator"
    spaces
    return ( )

intFunctionArgs :: GenParser Char st [ AST ]
intFunctionArgs = do
    args <- intFactor `sepBy` ( try intFunctionArgsSeparator )
    return args

intFunctionCall :: GenParser Char st AST
intFunctionCall = do
    functionName <- intIdentifier
    char '['
    spaces
    args <- ( try intFunctionArgs ) <|> return [ ]
    spaces
    char ']'
    spaces
    return ( ASTFunctionCall functionName args )


-- ─── EXPRESSION ─────────────────────────────────────────────────────────────────

intExpresson :: GenParser Char st AST
intExpresson =
    buildExpressionParser table intFactor <?> "expression" where

        table = [ negateParser ] : binaryTable

        negateParser =
            Prefix ( reservedOp "-" >> return ASTNegation )

        binaryTable = tableOf [ [ '?', '!' ]
                              , [ '^' ]
                              , [ '*', '/', '%' ]
                              , [ '+', '-' ]
                              ]

        tableOf xss =
            [ [ createOperatorParserFor x | x <- xs ] | xs <- xss ]

        createOperatorParserFor name =
            Infix operatorParser AssocLeft
            where
                operatorParser =
                    reservedOp [ name ] >> return ( ASTBinaryOperator opType )
                opType =
                    case name of '/' -> Div
                                 '+' -> Sum
                                 '-' -> Sub
                                 '*' -> Mul
                                 '%' -> Mod
                                 '^' -> Pow
                                 '?' -> Equ
                                 '!' -> NEq

-- ─── VERSUS ─────────────────────────────────────────────────────────────────────

intVersusSymbol :: GenParser Char st String
intVersusSymbol = do
    char '|'
    spaces
    return ""

intVersus :: GenParser Char st AST
intVersus = do
    parts <- intExpresson `sepBy` intVersusSymbol
    return $ if length parts == 1
        then parts !! 0
        else ASTVersus parts


-- ─── ASSIGNMENT ─────────────────────────────────────────────────────────────────

intAssignment :: GenParser Char st AST
intAssignment = do
    name <- intIdentifier
    char '='
    spaces
    value <- intExpresson
    return $ ASTAssignment name value


-- ─── ROOT ───────────────────────────────────────────────────────────────────────

intRoot :: GenParser Char st AST
intRoot = do
    spaces
    root <- try intAssignment <|> intVersus
    eof
    return root


-- ─── EXPOSED PARSER API ─────────────────────────────────────────────────────────

parseIntactus :: String -> Either ParseError AST
parseIntactus input =
    parse intRoot "" input


-- ────────────────────────────────────────────────────────────────────────────────
