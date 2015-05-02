module MyList where --MyList.hs

data List a b = Nil | Head a (List b a)  deriving (Show, Eq)

--длина списка
size :: List a b -> Int
size list = case list of
        Nil -> 0
        Head a tail -> 1 + (size tail)

--преобрзование списка по двум функциям   
dMap :: List a b -> (a -> c) -> (b -> d) -> List c d
dMap list f1 f2 = case list of
        Nil -> Nil
        Head a tail -> Head (f1 a) (dMap tail f2 f1)