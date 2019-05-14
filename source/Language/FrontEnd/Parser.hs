
module Language.FrontEnd.Parser ( parseIntactus ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import             Control.Monad ((>>))
import             Data.Char
import             Data.List
import             Data.List.Split ( splitOn )
import             Language.FrontEnd.AST
import  qualified  Text.ParserCombinators.Parsec.Token as Token
import             Text.ParserCombinators.Parsec
import             Text.ParserCombinators.Parsec.Expr
import             Text.ParserCombinators.Parsec.Language
import             Model

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
    value <- haskellNumber <?> "number"
    spaces
    return $ ASTNumber ( case value of Right x -> toFixedPrec50 x
                                       Left  x -> toFixedPrec50 x )
    where
        toFixedPrec50 x =
            read ( show x ) :: P50

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

intIdentifierPrettifyNames :: String -> String
intIdentifierPrettifyNames input =
    output where
        output =
            intercalate " " prettifiedNames
        names =
            splitOn " " input
        prettifiedNames =
            [ getSpecialNames $ intWordAllCaps name | name <- names ]

intWordAllCaps :: String -> String
intWordAllCaps word =
    ( toUpper $ head word ) : [ toLower x | x <- tail word ]

getSpecialNames :: String -> String
getSpecialNames name =
    case name of
        "Alpha'"    -> "Α"
        "Alpha"     -> "α"
        "Beta'"     -> "Β"
        "Beta"      -> "β"
        "Chi'"      -> "Χ"
        "Chi"       -> "χ"
        "Delta'"    -> "Δ"
        "Delta"     -> "δ"
        "E"         -> "e"
        "Epsilon'"  -> "Ε"
        "Epsilon"   -> "ε"
        "Eta'"      -> "Η"
        "Eta"       -> "η"
        "Gamma'"    -> "Γ"
        "Gamma"     -> "γ"
        "Iota'"     -> "Ι"
        "Iota"      -> "ι"
        "Kappa'"    -> "Κ"
        "Kappa"     -> "κ"
        "Lambda'"   -> "Λ"
        "Lambda"    -> "λ"
        "Mu'"       -> "Μ"
        "Mu"        -> "μ"
        "Nu'"       -> "Ν"
        "Nu"        -> "ν"
        "Omega'"    -> "Ω"
        "Omega"     -> "ω"
        "Omicron'"  -> "Ο"
        "Omicron"   -> "ο"
        "Phi'"      -> "Φ"
        "Phi"       -> "φ"
        "Pi'"       -> "Π"
        "Pi"        -> "π"
        "Psi'"      -> "Ψ"
        "Psi"       -> "ψ"
        "Rho'"      -> "Ρ"
        "Rho"       -> "ρ"
        "Sigma'"    -> "Σ"
        "Sigma"     -> "σ"
        "Tau'"      -> "Τ"
        "Tau"       -> "τ"
        "Theta'"    -> "Θ"
        "Theta"     -> "θ"
        "Upsilon'"  -> "Υ"
        "Upsilon"   -> "υ"
        "Xi'"       -> "Ξ"
        "Xi"        -> "ξ"
        "Zeta'"     -> "Ζ"
        "Zeta"      -> "ζ"
        _           -> name

intIdentifier :: GenParser Char st AST
intIdentifier = do
    firstChar <- letter
    name <- many ( intIdentifierLetterr <|> try intIdentifierSpacedPart )
    spaces
    return $ ASTIdentifier ( intIdentifierPrettifyNames ( intercalate ""
        ( intIdentifierJoiner firstChar name ) ) )

-- ─── VALUES ─────────────────────────────────────────────────────────────────────

intValues :: GenParser Char st AST
intValues =
    try intFunctionCall <|> intNumber <|> intIdentifier <?> "value"

-- ─── FACTOR ─────────────────────────────────────────────────────────────────────

intFactorWithParenthesis :: GenParser Char st AST
intFactorWithParenthesis = do
    char '(' <?> "left parenthesis"
    spaces
    value <- intExpresson
    char ')' <?> "right parenthesis"
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
    args <- intExpresson `sepBy` ( try intFunctionArgsSeparator )
    return args

intFunctionCall :: GenParser Char st AST
intFunctionCall = do
    functionName <- intIdentifier
    char '[' <?> "function argument left bracket"
    spaces
    args <- ( try intFunctionArgs ) <|> return [ ] <?> "function arguments"
    spaces
    char ']' <?> "function argument right bracket"
    spaces
    return ( ASTFunctionCall functionName args )


-- ─── EXPRESSION ─────────────────────────────────────────────────────────────────

intExpresson :: GenParser Char st AST
intExpresson =
    buildExpressionParser table intFactor <?> "expression" where
        table =
            [ negateParser ] : binaryTable
        negateParser =
            Prefix $ reservedOp "-" >> return ASTNegation
        binaryTable =
            tableOf [ [ '^' ]
                    , [ '*', '/', '%' ]
                    , [ '+', '-' ]
                    , [ '?', '!' ]
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
    char '|' <?> "versus operator"
    spaces
    return ""

intVersus :: GenParser Char st AST
intVersus = do
    parts <- intExpresson `sepBy` intVersusSymbol <?> "versus expressions"
    return $ if length parts == 1
        then parts !! 0
        else ASTVersus parts

-- ─── ASSIGNMENT ─────────────────────────────────────────────────────────────────

intAssignment :: GenParser Char st AST
intAssignment = do
    name <- intIdentifier <?> "assignment name"
    char '=' <?> "assignment operator (=)"
    spaces
    value <- intExpresson <?> "assignment value"
    return $ ASTAssignment name value

-- ─── ROOT ───────────────────────────────────────────────────────────────────────

intRoot :: GenParser Char st AST
intRoot = do
    spaces
    root <- try intAssignment <|> intVersus <?> "root value"
    eof
    return root

-- ─── EXPOSED PARSER API ─────────────────────────────────────────────────────────

parseIntactus :: String -> Either ParseError AST
parseIntactus input =
    parse intRoot "" input

-- ────────────────────────────────────────────────────────────────────────────────
