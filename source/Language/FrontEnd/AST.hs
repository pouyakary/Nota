
module Language.FrontEnd.AST where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Number.Fixed
import Model

-- ─── AST ELEMENTS ───────────────────────────────────────────────────────────────

data BinaryOperators = Sum | Sub | Div | Mul | Mod | Pow | Equ | NEq
    deriving ( Show, Eq )

data AST = ASTNumber            P50
         | ASTIdentifier        String
         | ASTBinaryOperator    BinaryOperators AST AST
         | ASTFunctionCall      AST [ AST ]
         | ASTVersus            [ AST ]
         | ASTNegation          AST
         | ASTAssignment        AST AST
         | ASTParenthesis       AST
           deriving ( Show, Eq )


-- ────────────────────────────────────────────────────────────────────────────────
