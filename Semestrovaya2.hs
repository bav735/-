module Semestrovaya2 where --Semestrovaya2.hs

--type representation
data Type = 
     T String
   | Arr Type Type
   deriving Eq
--shows type
instance Show (Type) where
   show (T s) = s
   show (Arr t1 t2) = 
      if t1 == t2 then show t1
      else "["++show t1++"=>"++show t2++"]"
           
--term representation
data Term =
     Var String Type
   | App Term Term Type
   | Abs String Type Term Type
   | Invalid String
   deriving Eq
--shows term
instance Show (Term) where
   show (te0) = if isValidTerm te0 then case te0 of  
      Var s t -> s ++ ":" ++ show t
      App te1 te2 ty -> "(" ++ show te1 ++ "->" ++ show te2 ++ "):" ++ show ty         
      Abs x tyx te tyr -> "(\\" ++ show(Var x tyx) ++ "." ++ show te ++ "):" ++ show tyr
   else case te0 of
      Invalid err -> err
      _ -> "Invalid term!"
   
--term validation function      
isValidTerm :: Term -> Bool
isValidTerm te0 = case te0 of
   Var s t -> True
   App te1 te2 ty -> case getTermType te1 of
      Arr ty1 ty2 -> if ty1 == getTermType te2 && ty2 == ty
         then isValidTerm te1 && isValidTerm te2
         else False
      _ -> False
   Abs x tyx te tyr -> case tyr of
      Arr ty1 ty2 -> if ty1 == tyx && ty2 == getTermType te
         then isValidTerm te
         else False
      _ -> False
   _ -> False
 
--get term type function
getTermType :: Term -> Type
getTermType te0 = case te0 of      
   Var y t -> t
   Abs y tyy te tyr -> tyr
   App te2 te3 tyr -> tyr
     
--substitution function
replaceVar :: Term -> String -> Type -> Term -> Term
replaceVar te0 x tyx te1 = case te0 of      
   Var y t -> if y == x
      then te1
      else te0
   Abs y tyy te tyr -> if x == y   
			then te0  
			else Abs y tyy (replaceVar te x tyx te1) tyr
   App te2 te3 tyr -> App (replaceVar te2 x tyx te1) (replaceVar te3 x tyx te1) tyr   

--evaluation function
eval :: Term -> Term
eval te0 = if isValidTerm te0
   then case te0 of
      Abs x tyx te tyr -> Abs x tyx (eval te) tyr
      App te1 te2 tyr ->  case te1 of      
         Abs x tyx te tyr -> eval(replaceVar te x tyx te2)	
         _ -> App (eval te1) (eval te2) tyr
      _ -> te0
   else Invalid "Couldn't evaluate - invalid term!"
     
--for testing
ty::Type
ty = Arr (T "Cat") (T "Dog") --type
te::Term
te = Abs "x" (T "Cat") (Var "y" (T "Dog")) (T "Pig") --check invalid
tred::Term
tred = App (Abs "x" (T "Note") (Var "x" (T "Book")) (Arr (T "Note") (T "Book"))) (Var "y" (T "Note")) (T "Book") --check reduce