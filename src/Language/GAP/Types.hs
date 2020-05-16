module Language.GAP.Types where

import Data.Maybe ()

data Stmt = Seq [Stmt]
          | Assign String Expr
          | If (Expr, Stmt) [(Expr, Stmt)] (Maybe Stmt)
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
             | RecordLit [(String, Expr)]
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
          | FuncCall Expr [Expr] [(String, Expr)]
          | List [Expr]
          | ListSlice Expr Expr
          | ListRange Expr (Maybe Expr) Expr
            deriving (Show, Eq)
