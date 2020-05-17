module Language.GAP.Parser where

import           System.IO
import           Data.Maybe
import           Data.List
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

printStmt :: Stmt -> String
printStmt (Seq stmts      ) = concat $ map (\x -> printStmt x ++ ";") stmts
printStmt (Assign var expr) = printAssign (var, expr)
printStmt (If (cond1, body1) elifs elseBody) =
  "if "
    ++ printExpr cond1
    ++ " then "
    ++ printStmt body1
    ++ " "
    ++ (concat $ map
         (\(cond, body) ->
           "elif " ++ printExpr cond ++ " then " ++ printStmt body ++ "; "
         )
         elifs
       )
    ++ (case elseBody of
         Just body -> "else " ++ printStmt body ++ "; "
         Nothing   -> ""
       )
    ++ " fi;"

printStmt (While cond body) =
  "while " ++ printExpr cond ++ " do " ++ printStmt body ++ "; od;"

printStmt (ExprStmt expr) = printExpr expr ++ ";"

printStmt (Return   expr) = "return " ++ printExpr expr ++ ";"

printExpr (Lit (BoolLit   b)) = show b
printExpr (Lit (StringLit s)) = show s
printExpr (Lit (IntLit    i)) = show i
printExpr (Lit (FloatLit  d)) = show d
printExpr (Lit (FuncDef args body)) =
  "function (" ++ intercalate ", " args ++ ") " ++ printStmt body ++ "; end"
printExpr (Lit (Lambda arg expr)) = arg ++ " -> " ++ printExpr expr
printExpr (Lit (RecordLit args)) =
  "rec(" ++ intercalate ", " (map printAssign args) ++ ")"
printExpr (Neg expr) = "-" ++ brack (printExpr expr)
printExpr (Not expr) = "not " ++ brack (printExpr expr)
printExpr (Binary op left right) =
  brack (printExpr left) ++ printOp op ++ brack (printExpr right)
printExpr (Var s) = s
printExpr (FuncCall f args namedArgs) =
  brack (printExpr f)
    ++ "("
    ++ intercalate ", " (map printExpr args)
    ++ if (length args > 0) && (length namedArgs > 0)
         then ":"
         else "" ++ intercalate ", " (map printAssign namedArgs) ++ ")"
printExpr (List l) = "[" ++ (intercalate ", " $ map printExpr l) ++ "]"
printExpr (ListSlice list slice) =
  printExpr list ++ "{" ++ printExpr slice ++ "}"
printExpr (ListRange first second end) = "["
                                          ++ printExpr first
                                          ++ case second of
                                               Just e -> ", " ++ printExpr e
                                               Nothing -> ""
                                          ++ ".."
                                          ++ printExpr end
                                          ++ "]"

printAssign (var, expr) = var ++ " := " ++ printExpr expr
brack s = "(" ++ s ++ ")"

printOp op = case op of
               Add -> "+"
               Subtract -> "-"
               Multiply -> "*"
               Divide -> "/"
               And -> " and "
               Or -> " or "
               Greater -> ">"
               GreaterEq -> ">="
               Less -> "<"
               LessEq -> "<="
               Equal -> "="
               NotEqual -> "<>"
               In -> " in "
               Mod -> " mod "
               Power -> "^"

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
