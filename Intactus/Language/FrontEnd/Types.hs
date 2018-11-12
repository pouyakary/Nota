
module Language.FrontEnd.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Scientific


-- ─── AST ELEMENTS ───────────────────────────────────────────────────────────────

data BinaryOperators = Sum | Sub | Div | Mul | Mod | Pow | Equ | NEq
    deriving ( Show, Eq )

data AST = ASTRoot              [ AST ]
         | ASTNumber            Scientific
         | ASTIdentifer         String
         | ASTBinaryOperator    BinaryOperators AST AST
         | ASTFunctionCall      AST [ AST ]
         | ASTVersus            [ AST ]
         | ASTNegation          AST
         | ASTAssignment        AST AST
           deriving ( Show, Eq )


-- ────────────────────────────────────────────────────────────────────────────────