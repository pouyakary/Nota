
module Language.FrontEnd.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Scientific

-- ─── AST ELEMENTS ───────────────────────────────────────────────────────────────

data BinaryOperators = Sum | Sub | Div | Mul | Mod | Pow | Equ | NEq
    deriving ( Show, Eq )

data AST = ASTNumber            Scientific
         | ASTIdentifer         String
         | ASTBinaryOperator    BinaryOperators AST AST
         | ASTFunctionCall      AST [ AST ]
         | ASTVersus            [ AST ]
         | ASTNegation          AST
         | ASTAssignment        AST AST
         | ASTParenthesis       AST
           deriving ( Show, Eq )


-- ────────────────────────────────────────────────────────────────────────────────
