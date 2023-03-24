{- 
    DEVOIR 2
    Auteurs : Ylli Fazlija, Rui Mota Carneiro
    Date : 21.03.23
    Description : Programme Haskell fournissant diverses fonctions servant d'ensembles.
-}

bound :: Int
bound = 1000

type Set = Int -> Bool

validSet::Set -> Set
validSet set x = set x && (x>=(-bound)) && (x<=bound)

instance Eq Set where
    (==) s1 s2 = check (-bound) where
        check x
            | x == bound + 1 = True
            | s1 x == s2 x = check (x+1)
            | otherwise = False
    (/=) s1 s2 = check (-bound) where
        check x
            | x == bound + 1 = False
            | s1 x == s2 x = check (x+1)
            | otherwise = True

--Displays a set as an understandable and clean text
instance Show Set where
    show set = "{" ++ show' (-bound) set False False False ++ "}" where
        show' x set previous isSingle remember
            | x == bound + 1 && previous = ".." ++ show (x-1)
            | x == bound + 1 && (isSingle || not previous) = ""
            | set x && remember = ", " ++ show' x set previous isSingle False
            | not (set x) && isSingle = show' (x+1) set False False True
            | not (set x) && not previous = show' (x+1) set False False remember
            | not (set x) && previous = ".." ++ show (x-1) ++ show' (x+1) set False False True
            | set x && isSingle = show' x set previous False False
            | set x && previous = show' (x+1) set True False False
            | set x && not previous = show x ++ show' (x+1) set True True False

--Bonus : It works, just be sure to write : subSet ⊆ mainSet, val `ϵ` set
(⊆) :: Set -> Set -> Bool
(⊆) sub main = check (-bound) where
    check x
        | x == bound + 1 = True
        | sub x = main x && check (x+1)
        | otherwise = check (x+1)

ϵ :: Int -> Set -> Bool
(ϵ) val set = elem' set val

emptySet::Set
emptySet x = False

elem' :: Set -> Int -> Bool
elem' = validSet

singleton :: Int -> Set
singleton x = validSet (==x)

union :: Set -> Set -> Set
union s1 s2 x = validSet s1 x || s2 x

intersect :: Set -> Set -> Set
intersect s1 s2 x = validSet s1 x && s2 x

diff :: Set -> Set -> Set
diff main sub x = validSet main x && not(sub x)

filter' :: Set -> (Int -> Bool) -> Set
filter' = intersect

all' :: Set -> (Int -> Bool) -> Bool
all' set predicat = intersect set predicat == predicat

any' :: Set -> (Int -> Bool) -> Bool
any' set predicat = intersect set predicat /= emptySet

map' :: Set -> (Int -> Int) -> Set
map' set function = validSet (check (-bound) emptySet) where
    check x resultSet
        | x == bound + 1 = resultSet
        | set x = check (x+1) resultSet `union` singleton (function x)
        | otherwise = check (x+1) resultSet

toString :: Set -> String
toString = show