module Language.GAP.Statement where

import           Language.GAP.Lexer

data Stmt = Seq [Stmt]
          | Assign String Expr
          | IfElif [(Expr, Stmt)]
          | IfElifElse [(Expr, Stmt)] Stmt
          | While Expr Stmt
          | ExprStmt Expr
          | Return Expr
            deriving (Show)

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
