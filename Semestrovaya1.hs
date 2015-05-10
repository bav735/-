module Semestrovaya1 where --Semestrovaya1.hs  

--variable representation
type Var a = a

--term representation
data Term = Var String | App Term Term | Abs (Var String) Term deriving (Eq, Ord, Read)
instance Show Term where
   show (Var x) = x
   show (Abs x t) = "\"" ++ x ++ "." ++ show t
   show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

--substitution function
replaceVar :: Term -> Var String -> Term -> Term
replaceVar t0 x t = case t0 of      
   Var y -> if y == x
      then t
      else t0
   Abs y t1 -> if x == y   
			then t0  
			else Abs y (replaceVar t1 x t)
   App t1 t2 -> App (replaceVar t1 x t) (replaceVar t2 x t)      

--evaluation function
eval :: Term -> Term
eval t0 = case t0 of
   Abs x t -> Abs x (eval t)
   App t1 t2 ->  case t1 of      
      Abs x t -> eval(replaceVar t x t2)	
      _ -> App (eval t1) (eval t2)      
   _ -> t0
   
--for testing
t1::Term
t1 = Abs ("x") (App (Var "x") (Var "y"))
t2::Term
t2 = Var "z"
t3::Term
t3 = App t1 t2