module GapParser where

import           System.IO
import           Control.Monad
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

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
            deriving (Show)

data Stmt = Seq [Stmt]
          | Assign String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | ExprStmt Expr
          | Return Expr
            deriving (Show)

-- Taken directly from GAPInfo.Keywords
gapKeywords =
  [ "and"
  , "atomic"
  , "break"
  , "continue"
  , "do"
  , "elif"
  , "else"
  , "end"
  , "false"
  , "fi"
  , "for"
  , "function"
  , "if"
  , "in"
  , "local"
  , "mod"
  , "not"
  , "od"
  , "or"
  , "readonly"
  , "readwrite"
  , "rec"
  , "repeat"
  , "return"
  , "then"
  , "true"
  , "until"
  , "while"
  , "quit"
  , "QUIT"
  , "IsBound"
  , "Unbind"
  , "TryNextMethod"
  , "Info"
  , "Assert"
  , ":="
  ]

gapOperators =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "^"
  , "~"
  , "="
  , "<>"
  , "<="
  , ">="
  , "<"
  , ">"
  , "and"
  , "or"
  , "not"
  , "mod"
  , "in"
  ]

languageDef = emptyDef { Token.commentStart    = ""
                       , Token.commentEnd      = ""
                       , Token.commentLine     = "#"
                       , Token.identStart      = letter
                       , Token.identLetter     = alphaNum <|> char '_'
                       , Token.reservedNames   = gapKeywords
                       , Token.reservedOpNames = gapOperators
                       , Token.caseSensitive   = True
                       , Token.opStart         = oneOf $ concat gapOperators
                       , Token.opLetter        = oneOf $ concat gapOperators
                       }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
commaSep = Token.commaSep lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
stringLiteral = Token.stringLiteral lexer
float = Token.float lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement = do
  list <- many1 statement'
  return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = do
  stmt <- try returnStmt <|> try ifStmt <|> try whileStmt <|> try assignStmt <|> try exprStmt
  semi
  return stmt

exprStmt = liftM ExprStmt expression

returnStmt = do
  reserved "return"
  expr <- expression
  return $ Return expr

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- expression
  reserved "then"
  stmt1 <- statement
  reserved "else"
  stmt2 <- statement
  reserved "fi"
  return $ If cond stmt1 stmt2

funcDef = do
  reserved "function"
  args <- parens (commaSep identifier)
  body <- statement
  reserved "end"
  return $ FuncDef args body

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- expression
  reserved "do"
  stmt <- statement
  return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  reserved ":="
  expr <- expression
  return $ Assign var expr

expression :: Parser Expr
expression = buildExpressionParser operators term

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

parseString :: String -> Stmt
parseString str = case parse whileParser "" str of
  Left  e -> error $ show e
  Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  case parse whileParser "" program of
    Left  e -> print e >> fail "parse error"
    Right r -> return r
