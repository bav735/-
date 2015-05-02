module FoldRL where --FoldRL.hs

--Task 1 
mapR :: (a -> b) -> [a] -> [b]
mapR f [] = []
mapR f (x:xs) = f x : foldr (\a bs -> (f a):bs) [] xs

mapL :: (a -> b) -> [a] -> [b]
mapL f [] = []
mapL f (x:xs) = f x : foldl (\bs a -> bs ++ [(f a)]) [] xs
--Task 1 

--Task 2
filter_ :: (a -> Bool) -> [a] -> [a]
filter_ f ys = case ys of
   [] -> []
   (x:xs) -> if f x
      then x : filter_ f xs
      else filter_ f xs
      
filterR :: (a -> Bool) -> [a] -> [a]
filterR f [] = []
filterR f (x:xs) = foldr (\y ys ->
      if f y then y:ys
      else ys
   ) [] xs
      
      
filterL :: (a -> Bool) -> [a] -> [a]
filterL f [] = []
filterL f (x:xs) = foldl (\ys y ->
      if (f y) then y:ys
      else ys
   ) [] xs         

concat_ :: [[a]] -> [a]
concat_ [] = []
concat_ (x:xs) = x ++ concat_ xs

concatR :: [[a]] -> [a]
concatR [] = []
concatR xs = foldl (\x y -> x ++ y) [] xs

concatL :: [[a]] -> [a]
concatL [] = []
concatL xs = foldr (\x y -> x ++ y) [] xs

concatMap_ :: (a -> [b]) -> [a] -> [b]
concatMap_ f [] = []
concatMap_ f (x:xs) =  f x ++ concatMap_ f xs

concatMapR :: (a -> [b]) -> [a] -> [b]
concatMapR f [] = []
concatMapR f xs =  foldr (\x y -> (f x) ++ y) [] xs

concatMapL :: (a -> [b]) -> [a] -> [b]
concatMapL f [] = []
concatMapL f xs =  foldl (\y x -> (f x) ++ y) [] xs
--Task 2