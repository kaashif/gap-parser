import           Test.Hspec
import           Language.GAP.Parser
import           Language.GAP.Types
import           Control.Monad

checkExample (description, str, ast) =
  it description $ (parseString str) `shouldBe` ast

examples =
  [ ("parses int assign"  , "a := 1;"  , (Assign "a" (Lit $ IntLit 1)))
  , ("parses float assign", "a := 1.0;", (Assign "a" (Lit $ FloatLit 1.0)))
  , ( "parses string assign"
    , "a := \"test string\";"
    , (Assign "a" (Lit $ StringLit "test string"))
    )
  , ( "parses assign to alphanumeric variable"
    , "test123 := 1.0;"
    , (Assign "test123" (Lit $ FloatLit 1.0))
    )
  , ( "parses single expression function literal"
    , "function(x) return x; end;"
    , (ExprStmt $ Lit $ FuncDef ["x"] $ Return $ Var "x")
    )
  , ( "parses assign to single expression function literal"
    , "f := function(x) return x; end;"
    , (Assign "f" $ Lit $ FuncDef ["x"] $ Return $ Var "x")
    )
  , ( "parses assign to multi-line function literal"
    , "f := function(x)\
      \    return x;\
      \end;"
    , (Assign "f" $ Lit $ FuncDef ["x"] $ Return $ Var "x")
    )
  , ( "parses multi-expression function literal"
    , "function(x) x := 1; return x; end;"
    , (ExprStmt $ Lit $ FuncDef ["x"] $ Seq
        [Assign "x" (Lit $ IntLit 1), Return $ Var "x"]
      )
    )
  , ( "parses multiple argument function literal"
    , "function(arg1, arg2) return arg1; end;"
    , (ExprStmt $ Lit $ FuncDef ["arg1", "arg2"] $ Return $ Var "arg1")
    )
  , ( "parses function call"
    , "g(1);"
    , (ExprStmt $ FuncCall "g" [Lit $ IntLit 1])
    )
  ]

main :: IO ()
main = hspec $ describe "Language.GAP.Parser.parseString" $ mapM_
  checkExample
  examples
