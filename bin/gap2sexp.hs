import           Language.GAP.Parser            ( parseString )
import           Language.GAP.Types
import           Data.Ratio
import           Control.Applicative
import           Data.List


-- This is just some basic stuff to allow conversion to Rust source,
-- there's no consideration given to efficiency here.
data Cyclotomic = Cyclotomic { order :: Integer, coeffs :: [(Integer, Rational)] }
  deriving Show
data CyclotomicExpr = CycMany [CyclotomicExpr]
                    | CycSingle Cyclotomic
                    deriving Show

cyc2sexp (CycMany cycs) = "(list " ++ intercalate " " (map cyc2sexp cycs) ++ ")"
cyc2sexp (CycSingle (Cyclotomic n coeffs)) = "(cyclotomic (order " ++ show n ++ ") (coeffs " ++ intercalate " " (map coeff2sexp coeffs) ++ "))"

coeff2sexp (exp, rat) = "(coeff (exponent "++ show exp ++ ") (rational " ++ show (numerator rat) ++ " " ++ show (denominator rat) ++"))"

ast2sexp :: Stmt -> Maybe String
ast2sexp (ExprStmt expr) = do
  cycs <- parseManyOrSingle expr
  return $ cyc2sexp cycs

parseManyOrSingle :: Expr -> Maybe CyclotomicExpr
parseManyOrSingle (List exprs) = fmap CycMany $ mapM parseManyOrSingle exprs
parseManyOrSingle expr         = parseSingle expr

parseSingle :: Expr -> Maybe CyclotomicExpr
parseSingle expr = parseTerm expr <|> parseSumOfTerms expr

parseSumOfTerms (Binary op expr termExpr) = do
  lhs    <- parseTerm expr <|> parseSumOfTerms expr
  rhs    <- parseTerm termExpr
  negate <- case op of
    Add      -> Just False
    Subtract -> Just True
    _        -> Nothing
  case (lhs, rhs) of
    (CycSingle (Cyclotomic n leftCoeffs), CycSingle (Cyclotomic m rightCoeffs))
      -> if or [n == m, n == 1, m == 1]
        then return $ CycSingle $ Cyclotomic
          (max n m)
          (  leftCoeffs
          ++ (map
               (\(exp, coeff) -> (exp, if negate then -1 * coeff else coeff))
               rightCoeffs
             )
          ) -- assume no repeats
        else Nothing
    _ -> Nothing
parseSumOfTerms _ = Nothing

parseTerm :: Expr -> Maybe CyclotomicExpr
parseTerm expr = parseNoCoeffTerm expr <|> parseCoeffTerm expr

parseCoeffTerm :: Expr -> Maybe CyclotomicExpr
parseCoeffTerm (Binary Multiply rationalExpr noCoeffTermExpr) = do
  rational    <- parseRational rationalExpr
  noCoeffTerm <- parseNoCoeffTerm noCoeffTermExpr
  case noCoeffTerm of
    CycSingle (Cyclotomic n [(k, 1)]) ->
      return $ CycSingle $ Cyclotomic n [(k, rational)]
    _ -> Nothing
parseCoeffTerm (Neg expr) =
  parseCoeffTerm (Binary Multiply (Lit $ IntLit $ -1) expr)
parseCoeffTerm _ = Nothing

parseRational :: Expr -> Maybe Rational
parseRational (Binary Divide (Lit (IntLit num)) (Lit (IntLit denom))) =
  return $ num % denom
parseRational (Binary Divide (Neg (Lit (IntLit num))) (Lit (IntLit denom))) =
  return $ -num % denom
parseRational (Lit (IntLit n)) = return $ n % 1
parseRational (Neg (Lit (IntLit n))) = return $ -n % 1
parseRational _                = Nothing

parseRationalCyc expr = do
  rational <- parseRational expr
  return $ CycSingle $ Cyclotomic 1 [(0, rational)]

-- I know a rational on its own isn't really "no coeff"
parseNoCoeffTerm expr =
  parsePrimitiveRoot expr <|> parseRootPower expr <|> parseRationalCyc expr

parsePrimitiveRoot :: Expr -> Maybe CyclotomicExpr
parsePrimitiveRoot (FuncCall (Var "E") [Lit (IntLit n)] []) =
  Just $ CycSingle $ Cyclotomic n [(1, 1)]
parsePrimitiveRoot _ = Nothing

parseRootPower :: Expr -> Maybe CyclotomicExpr
parseRootPower (Binary Power root (Lit (IntLit k))) = do
  (CycSingle (Cyclotomic n _)) <- parsePrimitiveRoot root
  return $ CycSingle $ Cyclotomic n [(k, 1)]
parseRootPower _ = Nothing

main :: IO ()
main = do
  gapSrc <- getContents
  let ast = parseString gapSrc
  case ast2sexp ast of
    Just sexp -> putStrLn sexp
    Nothing   -> putStrLn $ "failed, ast is: " ++ show ast
