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
 where
  f [x] = x
  f xs  = Seq xs

statement :: Parser Stmt
statement = stmt <* many1 semi
 where
  stmt = returnStmt <|> ifStmt <|> whileStmt <|> try assignStmt <|> exprStmt

-- The only statements that evaluate to something are expressions
exprStmt = ExprStmt <$> expression

-- Note: return doesn't evaluate to a value, it's not an expression
returnStmt = pure Return <* reserved "return" <*> expression


ifStmt :: Parser Stmt
ifStmt =
  If
    <$> ((,) <$> (reserved "if" *> expression) <* reserved "then" <*> statements
        )
    <*> elifConds
    <*> elseBody
    <*  reserved "fi"
 where
  elifConds =
    many
      $   try
      $   (,)
      <$> (reserved "elif" *> expression)
      <*  reserved "then"
      <*> statements
  elseBody = optionMaybe $ try $ reserved "else" *> statements

whileStmt :: Parser Stmt
whileStmt =
  (pure While)
    <*  (reserved "while")
    <*> expression
    <*  reserved "do"
    <*> statements
    <*  reserved "od"

assignStmt :: Parser Stmt
assignStmt = Assign <$> identifier <* reservedOp ":=" <*> expression

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

argLists = many1 argList

argList =
  parens
    $   try ((,) [] <$> try options)
    <|> try ((,) <$> (commaSep1 expression) <* colon <*> options)
    <|> try ((,) <$> (commaSep expression) <*> pure [])

options = commaSep1 funcOption

funcOption = (,) <$> identifier <* reservedOp ":=" <*> expression

funcCallExpr = foldl (\x (y, z) -> FuncCall x y z) <$> notFuncCall <*> argLists

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

funcLit =
  FuncDef
    <$> (reserved "function" *> parens (commaSep identifier))
    <*> statements
    <*  reserved "end"

lambdaLit = Lambda <$> identifier <* reservedOp "->" <*> expression

recordLit = RecordLit <$> (reserved "rec" *> parens opts)
 where
  opts = commaSep opt
  opt  = (,) <$> identifier <* reservedOp ":=" <*> expression
