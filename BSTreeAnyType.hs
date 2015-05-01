module BSTreeAnyType where --BSTreeAnyType.hs

data Tree a = Any a (Tree a) (Tree a) | Nil deriving (Show, Eq)

tHeight :: Tree a -> Int
tHeight t = case t of
  Nil -> 0
  Any a left right -> ((max (tHeight left) (tHeight right)) + 1)  

tMap :: Tree a -> (a -> b) -> Tree b
tMap t f = case t of
  Nil -> Nil
  Any a left right -> Any (f a) (tMap left f) (tMap right f)