import           Test.Hspec
import           Language.GAP.Parser
import           Language.GAP.Types
import           Control.Monad

checkExample (description, str, ast) =
  it description $ (parseString str) `shouldBe` ast

-- Examples I wrote as I was writing the parser
myExamples =
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
    , (ExprStmt $ FuncCall (Var "g") [Lit $ IntLit 1])
    )
  , ( "parses lambda with spaces"
    , "i -> i+1;"
    , (ExprStmt $ Lit $ Lambda "i" $ Binary Add (Var "i") (Lit $ IntLit 1))
    )
  , ( "parses lambda without spaces"
    , "i->i+1;"
    , (ExprStmt $ Lit $ Lambda "i" $ Binary Add (Var "i") (Lit $ IntLit 1))
    )
  , ( "parses list map"
    , "List([1,2,3], x -> x+1);"
    , (ExprStmt $ FuncCall
        (Var "List")
        [ List [Lit $ IntLit 1, Lit $ IntLit 2, Lit $ IntLit 3]
        , Lit $ Lambda "x" $ Binary Add (Var "x") (Lit $ IntLit 1)
        ]
      )
    )
  ]

-- Examples taken directly from https://www.gap-system.org/Manuals/doc/ref/chap4.html
gapExamples =
  [ ( "4.1 arithmetic"
    , "1 + 2 * 3;"
    , ExprStmt $ Binary Add (Lit $ IntLit 1) $ Binary Multiply
                                                      (Lit $ IntLit 2)
                                                      (Lit $ IntLit 3)
    )
  , ( "4.4 if single line"
    , "if i<0 then a:=-i;else a:=i;fi;"
    , IfElifElse
      [(Binary Less (Var "i") (Lit $ IntLit 0), Assign "a" (Neg $ Var "i"))]
      (Assign "a" $ Var "i")
    )
  , ( "4.4 if multiline"
    , "if i < 0 then   # if i is negative\n\
      \  a := -i;      #   take its additive inverse\n\
      \else            # otherwise\n\
      \  a := i;       #   take itself\n\
      \fi;"
    , IfElifElse
      [(Binary Less (Var "i") (Lit $ IntLit 0), Assign "a" (Neg $ Var "i"))]
      (Assign "a" $ Var "i")
    )
  , ( "4.5 record member access"
    , "keys:=SortedList( GAPInfo.Keywords );; l:=Length( keys );;"
    , Seq
      [ Assign "keys" $ FuncCall (Var "SortedList") $ [Var "GAPInfo.Keywords"]
      , Assign "l" $ FuncCall (Var "Length") $ [Var "keys"]
      ]
    )
  , ( "4.5 list range and slicing"
    , "arr:= List( [ 0 .. Int( l/4 )-1 ], i-> keys{ 4*i + [ 1 .. 4 ] } );;"
    , Assign "arr" $ FuncCall
      (Var "List")
      [ ListRange
        (Lit $ IntLit 0)
        Nothing
        (Binary
          Subtract
          (FuncCall (Var "Int") $ [(Binary Divide (Var "l") (Lit $ IntLit 4))])
          (Lit $ IntLit 1)
        )
      , Lit $ Lambda "i" $ ListSlice
        (Var "keys")
        (Binary Add
                (Binary Multiply (Lit $ IntLit 4) (Var "i"))
                (ListRange (Lit $ IntLit 1) Nothing (Lit $ IntLit 4))
        )
      ]
    )
  ]

exampleSets = [("myExamples", myExamples), ("gapExamples", gapExamples)]

checkExampleSet topName (exName, examples) =
  describe (concat [topName, ":", exName]) $ mapM_ checkExample examples

main :: IO ()
main =
  hspec $ mapM_ (checkExampleSet "Language.GAP.Parser.parseString") exampleSets
