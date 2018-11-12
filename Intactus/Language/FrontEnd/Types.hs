
module Language.FrontEnd.Types where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Data.Scientific


-- ─── AST ELEMENTS ───────────────────────────────────────────────────────────────

data BinaryOperators =
    Sum | Sub | Div | Mul | Mod | Pow | Equals | NotEquals
    deriving ( Eq, Show )

data AST = ASTRoot              [ AST ]
         | ASTNumber            Scientific
         | ASTIdentifer         String
         | ASTBinaryOperator    BinaryOperators AST AST
         | ASTFunctionCall      String [ AST ]
         | ASTVersus            [ AST ]
         | ASTNegation          AST
         | ASTAssignment        AST AST
           deriving ( Eq, Show )


-- ────────────────────────────────────────────────────────────────────────────────
