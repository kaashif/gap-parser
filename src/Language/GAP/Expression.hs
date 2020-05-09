module Language.GAP.Expression where

import           System.IO
import           Control.Monad
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import           Language.GAP.Statement         ( Stmt )
import           Language.GAP.Lexer

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

funcDef = do
  reserved "function"
  args <- parens (commaSep identifier)
  body <- statement
  reserved "end"
  return $ FuncDef args body

expression :: Parser Expr
expression =
  try funcCallExpr <|> try listExpr <|> buildExpressionParser operators term

listExpr = squares $ do
  exprs <- commaSep expression
  return $ List exprs

funcCallExpr = do
  function <- identifier
  args     <- parens $ commaSep expression
  return $ FuncCall function args

operators =
  [ [Infix (reservedOp "^" >> return (Binary Power)) AssocNone]
  , [ Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft
    , Infix (reservedOp "/" >> return (Binary Divide))   AssocLeft
    , Infix (reservedOp "mod" >> return (Binary Mod))    AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Binary Add))      AssocLeft
    , Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft
    ]
  , [Infix (reservedOp "in" >> return (Binary In)) AssocNone]
  , [ Infix (reservedOp "=" >> return (Binary Equal))      AssocNone
    , Infix (reservedOp "<>" >> return (Binary NotEqual))  AssocNone
    , Infix (reservedOp "<" >> return (Binary Less))       AssocNone
    , Infix (reservedOp "<=" >> return (Binary LessEq))    AssocNone
    , Infix (reservedOp ">" >> return (Binary Greater))    AssocNone
    , Infix (reservedOp ">=" >> return (Binary GreaterEq)) AssocNone
    ]
  , [Prefix (reservedOp "not" >> return (Not))]
  , [ Infix (reservedOp "and" >> return (Binary And)) AssocLeft
    , Infix (reservedOp "or" >> return (Binary Or))   AssocLeft
    ]
  ]

term = parens expression <|> liftM Var identifier <|> liftM Lit literal

literal =
  (reserved "true" >> return (BoolLit True))
    <|> try (reserved "false" >> return (BoolLit False))
    <|> funcDef
    <|> fmap StringLit stringLiteral
    <|> try (fmap FloatLit float)
    <|> fmap IntLit integer
