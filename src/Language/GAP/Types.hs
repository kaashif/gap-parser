module Language.GAP.Types where

data Stmt = Seq [Stmt]
          | Assign String Expr
          | IfElif [(Expr, Stmt)]
          | IfElifElse [(Expr, Stmt)] Stmt
          | While Expr Stmt
          | ExprStmt Expr
          | Return Expr
            deriving (Show)

data Literal = BoolLit Bool
             | StringLit String
             | IntLit Integer
             | FloatLit Double
             | FuncDef [String] Stmt
               deriving (Show)

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
             deriving (Show)

data Expr = Lit Literal
          | Not Expr
          | Binary BinOp Expr Expr
          | Var String
          | FuncCall String [Expr]
          | List [Expr]
            deriving (Show)