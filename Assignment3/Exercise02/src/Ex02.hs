{- ccoady Ciaran Coady -}
module Ex02 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

type EDict = Dict String Double

v42 = Val 42 ; j42 = Just v42

-- Part 1 : Evaluating Expressions -- (60 test marks, worth 15 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.

eval :: EDict -> Expr -> Maybe Double
--Base Cases
eval _ (Val x) = Just x 
eval _ (Add (Val x) (Val y)) = Just (x + y)
eval _ (Sub (Val x) (Val y)) = Just (x - y)
eval _ (Mul (Val x) (Val y)) = Just (x * y)
eval _ (Dvd _ (Val 0.0)) = Nothing
eval _ (Dvd (Val x) (Val y)) = Just (x / y)
eval d (Def x (Val y) e2) = eval (define d x y) e2
eval d (Def _ _ _) = Nothing
eval d (Var x) = find d x
eval [] e = Nothing              



         
eval ((s,v):ds) (Add (Var x) (Val y)) | x == s = eval [("Simplified",0.0)] (Add (Val v) (Val y))
                                      | otherwise = eval ds (Add (Var x) (Val y))

eval ((s,v):ds) (Add (Val x) (Var y)) | y == s = eval [("Simplified",0.0)] (Add (Val x) (Val v))
                                      | otherwise = eval ds (Add (Val x) (Var y))

eval ((s,v):ds) (Add (Var x) (Var y)) | x == s = eval ds (Add (Val v) (Var y))
                                      | y == s = eval ds (Add (Var x) (Val v))
                                      | otherwise = eval ds (Add (Var x) (Var y))

eval d (Add _ _) = Nothing



eval ((s,v):ds) (Sub (Var x) (Val y)) | x == s = eval [("Simplified",0.0)] (Sub (Val v) (Val y))
                                      | otherwise = eval ds (Sub (Var x) (Val y))

eval ((s,v):ds) (Sub (Val x) (Var y)) | y == s = eval [("Simplified",0.0)] (Sub (Val x) (Val v))
                                      | otherwise = eval ds (Sub (Val x) (Var y))

eval ((s,v):ds) (Sub (Var x) (Var y)) | x == s = eval ds (Sub (Val v) (Var y))
                                      | y == s = eval ds (Sub (Var x) (Val v))
                                      | otherwise = eval ds (Sub (Var x) (Var y))

eval d (Sub _ _) = Nothing



eval ((s,v):ds) (Mul (Var x) (Val y)) | x == s = eval [("Simplified",0.0)] (Mul (Val v) (Val y))
                                      | otherwise = eval ds (Mul (Var x) (Val y))

eval ((s,v):ds) (Mul (Val x) (Var y)) | y == s = eval [("Simplified",0.0)] (Mul (Val x) (Val v))
                                      | otherwise = eval ds (Mul (Val x) (Var y))

eval ((s,v):ds) (Mul (Var x) (Var y)) | x == s = eval ds (Mul (Val v) (Var y))
                                      | y == s = eval ds (Mul (Var x) (Val v))
                                      | otherwise = eval ds (Mul (Var x) (Var y))

eval d (Mul _ _) = Nothing



eval ((s,v):ds) (Dvd (Var x) (Val y)) | x == s = eval [("Simplified",0.0)] (Dvd (Val v) (Val y))
                                      | otherwise = eval ds (Dvd (Var x) (Val y))

eval ((s,v):ds) (Dvd (Val x) (Var y)) | y == s = eval [("Simplified",0.0)] (Dvd (Val x) (Val v))
                                      | otherwise = eval ds (Dvd (Val x) (Var y))

eval ((s,v):ds) (Dvd (Var x) (Var y)) | x == s = eval ds (Dvd (Val v) (Var y))
                                      | y == s = eval ds (Dvd (Var x) (Val v))
                                      | otherwise = eval ds (Dvd (Var x) (Var y))

eval d (Dvd _ _) = Nothing


-- Part 2 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x + y            =  y + x         Law 1
  x + (y + z)      =  (x + y) + z   Law 2
  x - (y + z)      =  (x - y) - z   Law 3
  (x + y)*(x - y)  =  x*x - y*y     Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}


law1 :: Expr -> Maybe Expr
law1 (Add x y) = Just (Add y x)
law1 _ = Nothing

law2 :: Expr -> Maybe Expr
law2 (Add x (Add y z)) = Just (Add (Add x y) z)
law2 _ = Nothing

law3 :: Expr -> Maybe Expr
law3 (Sub x (Add y z)) = Just (Sub (Sub x y) z)
law3 _ = Nothing

law4 :: Expr -> Maybe Expr
law4 (Mul (Add x y) (Sub a b)) = 
                                if x == a && y == b
                                  then Just (Sub (Mul x x) (Mul y y))
                                  else Nothing
law4 _ = Nothing
