
module Language.FrontEnd.Parser ( parseIntactus ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import             Control.Monad ((>>))
import             Data.Char
import             Data.List
import             Data.List.Split ( splitOn )
import             Data.Scientific
import             Language.FrontEnd.AST
import  qualified  Text.ParserCombinators.Parsec.Token as Token
import             Text.ParserCombinators.Parsec
import             Text.ParserCombinators.Parsec.Expr
import             Text.ParserCombinators.Parsec.Language

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
        "Alpha"     -> "α"
        "Beta"      -> "β"
        "Gamma"     -> "γ"
        "Delta"     -> "δ"
        "Epsilon"   -> "ε"
        "Zeta"      -> "ζ"
        "Eta"       -> "η"
        "Theta"     -> "θ"
        "Iota"      -> "ι"
        "Kappa"     -> "κ"
        "Lambda"    -> "λ"
        "Mu"        -> "μ"
        "Nu"        -> "ν"
        "Xi"        -> "ξ"
        "Omicron"   -> "ο"
        "Pi"        -> "π"
        "Rho"       -> "ρ"
        "Sigma"     -> "σ"
        "Tau"       -> "τ"
        "Upsilon"   -> "υ"
        "Phi"       -> "φ"
        "Chi"       -> "χ"
        "Psi"       -> "ψ"
        "Omega"     -> "ω"
        "Alpha'"    -> "Α"
        "Beta'"     -> "Β"
        "Gamma'"    -> "Γ"
        "Delta'"    -> "Δ"
        "Epsilon'"  -> "Ε"
        "Zeta'"     -> "Ζ"
        "Eta'"      -> "Η"
        "Theta'"    -> "Θ"
        "Iota'"     -> "Ι"
        "Kappa'"    -> "Κ"
        "Lambda'"   -> "Λ"
        "Mu'"       -> "Μ"
        "Nu'"       -> "Ν"
        "Xi'"       -> "Ξ"
        "Omicron'"  -> "Ο"
        "Pi'"       -> "Π"
        "Rho'"      -> "Ρ"
        "Sigma'"    -> "Σ"
        "Tau'"      -> "Τ"
        "Upsilon'"  -> "Υ"
        "Phi'"      -> "Φ"
        "Chi'"      -> "Χ"
        "Psi'"      -> "Ψ"
        "Omega'"    -> "Ω"
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
