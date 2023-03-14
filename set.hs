{- 
    DEVOIR 2
    Auteurs : Ylli Fazlija, Rui Mota Carneiro
    Date : 14.03.23
    Description : Programme Haskell fournissant diverses fonctions servant d'ensembles.
-}

bound :: Int
bound = 1000

type Set = a -> Bool | a <- [1..10]


set1 :: Set
set1 = (>3)

elem' :: Set -> Int -> Bool
elem' set = set

singleton' :: Int -> Set
singleton' x = (==x) :: Set


{-
union :: Set -> Set -> Set
intersect :: Set -> Set -> Set
diff :: Set -> Set -> Set
filter :: Set -> (Int -> Bool) -> Set
all :: Set -> (Int -> Bool) -> Bool
any :: Set -> (Int -> Bool) -> Bool
map :: Set -> (Int -> Int) -> Set
-}