module Language.GAP.Parser where

import           System.IO
import           Data.Maybe
import           Control.Monad
import           Control.Applicative     hiding ( (<|>)
                                                , many
                                                )
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import           Language.GAP.Lexer
import           Language.GAP.Types

whileParser :: Parser Stmt
whileParser = whiteSpace >> statements

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

-- Statements - expressions plus things that don't evaluate to values

statements = f <$> many1 statement
  where f list = if length list == 1 then head list else Seq list

statement :: Parser Stmt
statement = stmt <* many1 semi
  where stmt = returnStmt <|> ifStmt <|> whileStmt <|> assignStmt <|> exprStmt

-- The only statements that evaluate to something are expressions
exprStmt = ExprStmt <$> expression

-- Note: return doesn't evaluate to a value, it's not an expression
returnStmt = pure Return <* reserved "return" <*> expression

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- expression
  reserved "then"
  stmt1     <- statements

  elifConds <- many $ try $ do
    reserved "elif"
    elifCond <- expression
    reserved "then"
    elifBody <- statements
    return (elifCond, elifBody)

  elseBody <- optionMaybe $ try $ do
    reserved "else"
    body <- statements
    return body

  reserved "fi"

  return $ case elseBody of
    Just body -> IfElifElse ((cond, stmt1) : elifConds) body
    Nothing   -> IfElif $ (cond, stmt1) : elifConds

whileStmt :: Parser Stmt
whileStmt =
  (pure While)
    <*  (reserved "while")
    <*> expression
    <*  reserved "do"
    <*> statements
    <*  reserved "od"

assignStmt :: Parser Stmt
assignStmt = do
  var <- try $ do
    var <- identifier
    reservedOp ":="
    return var
  expr <- expression
  return $ Assign var expr

-- Expressions - things that evaluate to values

expression :: Parser Expr
expression = buildExpressionParser operators term

listExpr = squares $ List <$> (commaSep expression)

listSlice = foldl ListSlice <$> notListSlice <*> many1 sliceArg
  where sliceArg = braces expression

-- expression of the form [first, second .. last] or [first .. last]
listRange =
  squares
    $   ListRange
    <$> expression
    <*> (optionMaybe $ try (comma >> expression))
    <*  (reservedOp "..")
    <*> expression

funcCallExpr = foldl (\x (y, z) -> FuncCall x y z) <$> notFuncCall <*> argLists
 where
  argLists = many1 argList
  argList  = parens $ do
    args <- commaSep expression
    opts <- optionMaybe
      $ if null args then try options else try colon >> try options
    let realOpts = fromMaybe [] opts
    return (args, realOpts)
  options = commaSep option
  option  = do
    var <- try $ do
      var <- identifier
      reservedOp ":="
      return var
    expr <- expression
    return (var, expr)


operators =
  [ [Infix (reservedOp "^" >> return (Binary Power)) AssocNone]
  , [Prefix (reservedOp "-" >> return (Neg))]
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

term = try funcCallExpr <|> try listSlice <|> okTerms

-- Secretly we know that a listSlice can never give a function, only a
-- list, so a listSlice is never the first term of a function call
-- chain.
-- TODO: don't use that information! It's not the parser's job to
-- typecheck!
notFuncCall = okTerms

notListSlice = okTerms <|> try funcCallExpr

okTerms =
  parens expression
    <|> try listExpr
    <|> try listRange
    <|> try (Lit <$> literal)
    <|> try (Var <$> identifier)

-- Literals

literal =
  try (reserved "true" >> return (BoolLit True))
    <|> try (reserved "false" >> return (BoolLit False))
    <|> try funcLit
    <|> try (fmap StringLit stringLiteral)
    <|> try (fmap FloatLit float)
    <|> try (fmap IntLit integer)
    <|> try lambdaLit
    <|> try recordLit

funcLit = do
  reserved "function"
  args <- parens (commaSep identifier)
  body <- statements
  reserved "end"
  return $ FuncDef args body

lambdaLit = do
  arg <- try $ do
    arg <- identifier
    reservedOp "->"
    return arg
  ret <- expression
  return $ Lambda arg ret

recordLit = RecordLit <$> (reserved "rec" *> parens opts)
  where opts = commaSep opt
        opt = curry id <$> identifier <* reservedOp ":=" <*> expression
