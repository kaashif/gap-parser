module Language.GAP.Lexer where

import           System.IO
import           Control.Monad
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

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
  , ":="
  , "->"
  ]

languageDef = emptyDef { Token.commentStart    = ""
                       , Token.commentEnd      = ""
                       , Token.commentLine     = "#"
                       , Token.identStart      = letter
                       , Token.identLetter     = alphaNum <|> char '_'
                       , Token.reservedNames   = gapKeywords
                       , Token.reservedOpNames = gapOperators
                       , Token.caseSensitive   = True
                       , Token.opStart         = oneOf $ map head gapOperators
                       , Token.opLetter        = oneOf $ concat $ map tail gapOperators
                       }

lexer :: Token.TokenParser u
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
squares = Token.brackets lexer
