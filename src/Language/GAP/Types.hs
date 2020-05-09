module Language.GAP.Types where

import Data.Maybe

data Stmt = Seq [Stmt]
          | Assign String Expr
          | IfElif [(Expr, Stmt)]
          | IfElifElse [(Expr, Stmt)] Stmt
          | While Expr Stmt
          | ExprStmt Expr
          | Return Expr
            deriving (Show, Eq)

data Literal = BoolLit Bool
             | StringLit String
             | IntLit Integer
             | FloatLit Double
             | FuncDef [String] Stmt
             | Lambda String Expr
               deriving (Show, Eq)

data BinOp = Add
           | Subtract
           | Multiply
           | Divide
           | And
           | Or
           | Greater
           | GreaterEq
           | Less
           | LessEq
           | Equal
           | NotEqual
           | In
           | Mod
           | Power
             deriving (Show, Eq)

data Expr = Lit Literal
          | Neg Expr
          | Not Expr
          | Binary BinOp Expr Expr
          | Var String
          | FuncCall String [Expr]
          | List [Expr]
          | ListSlice Expr Expr
          | ListRange Expr (Maybe Expr) Expr
            deriving (Show, Eq)
