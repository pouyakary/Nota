
module Language.FrontEnd.AST where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Scientific

-- ─── AST ELEMENTS ───────────────────────────────────────────────────────────────

data BinaryOperators = Sum | Sub | Div | Mul | Mod | Pow | Equ | NEq
    deriving ( Show, Eq )

data AST = ASTNumber            Scientific
         | ASTIdentifier        String
         | ASTBinaryOperator    BinaryOperators AST AST
         | ASTFunctionCall      AST [ AST ]
         | ASTVersus            [ AST ]
         | ASTNegation          AST
         | ASTAssignment        AST AST
         | ASTParenthesis       AST
           deriving ( Show, Eq )


-- ────────────────────────────────────────────────────────────────────────────────
