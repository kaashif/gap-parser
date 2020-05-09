module Language.GAP.Parser where

import           System.IO
import           Control.Monad
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import           Language.GAP.Lexer
import           Language.GAP.Types

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

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

statement = do
  list <- many1 statement'
  return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = do
  stmt <- returnStmt <|> ifStmt <|> whileStmt <|> assignStmt <|> exprStmt
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
  stmt1     <- statement

  elifConds <- many $ try $ do
    reserved "elif"
    elifCond <- expression
    reserved "then"
    elifBody <- statement
    return (elifCond, elifBody)

  elseBody <- optionMaybe $ try $ do
    reserved "else"
    body <- statement
    return body

  reserved "fi"

  return $ case elseBody of
    Just body -> IfElifElse ((cond, stmt1) : elifConds) body
    Nothing   -> IfElif $ (cond, stmt1) : elifConds

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- expression
  reserved "do"
  stmt <- statement
  reserved "od"
  return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
  var <- try $ do
    var <- identifier
    reserved ":="
    return var
  expr <- expression
  return $ Assign var expr

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
