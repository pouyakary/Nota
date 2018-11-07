
module Language.FrontEnd.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Scientific

-- ─── AST ELEMENTS ───────────────────────────────────────────────────────────────

data BinaryOperators = Sum | Div | Mul | Mod | Pow
    deriving (Eq, Show)

data UnaryOperators = Neg
    deriving (Eq, Show)

data AST = ASTNumber            Scientific
         | ASTIdentifer         String
         | ASTBinaryOperator    BinaryOperators AST AST
         | ASTUnaryOperator     UnaryOperators AST
         | ASTFunction          String [ AST ]
         | ASTVersus            [ AST ]
           deriving ( Eq, Show )

-- ────────────────────────────────────────────────────────────────────────────────
