module BSTree where --BSTree.hs

data Tree = Node{left::Tree, right::Tree, value::Int}|Nil deriving (Show)

tHeight :: Tree -> Int
tHeight t = case t of
  Nil -> 0
  node -> (max (tHeight(left node)) (tHeight(right node) + 1))  

tSum :: Tree -> Int
tSum t = case t of
  Nil -> 0
  node -> ((value node) + tSum(left node) + tSum(right node))  

find :: Tree -> Int -> Bool
find t v = case t of
  Nil -> False
  node -> if ((value node) == v)
    then True
    else if (value node) > v
      then find (left node) v
      else find (right node) v