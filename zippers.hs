-- zippers
-- a simple list zipper allows replacement and insertion
-- the zipper represents a list with a current point of focus (cursor)
-- the elements to the left and right of the focus are stored as separate lists

data Zipper a = Zipper [a] [a] deriving (Show)

instance Foldable Zipper where
    foldr f z (Zipper ls rs) = foldr f (foldr f z rs) ls

instance Functor Zipper where
    fmap f (Zipper ls rs) = Zipper (map f ls) (map f rs)

makeZipper :: [a] -> Zipper a
makeZipper xs = Zipper [] xs

current :: Zipper a -> a
current (Zipper ls (e:rs)) = e

toList :: Zipper a -> [a]
toList (Zipper ls rs) = (reverse ls) ++ rs

left :: Zipper a -> Zipper a
left (Zipper (e:ls) rs) = Zipper ls (e:rs)
left (Zipper [] _) = errorWithoutStackTrace "no left"

right :: Zipper a -> Zipper a
right (Zipper ls (e:rs)) = Zipper (e:ls) rs
right (Zipper _ []) = errorWithoutStackTrace "no right"

replace :: a -> Zipper a -> Zipper a
replace e (Zipper ls (_:rs)) = Zipper ls (e:rs)

insert :: a -> Zipper a -> Zipper a
insert e (Zipper ls rs) = Zipper ls (e:rs)

remove :: Zipper a -> Zipper a
remove (Zipper ls (_:rs)) = Zipper ls rs
remove (Zipper _ []) = errorWithoutStackTrace "no right"

-- using zipper it is easy to find all pairs order dependent
allPairs :: Zipper a -> [(a,a)]
allPairs (Zipper _ []) = []
allPairs z@(Zipper ls (e:rs)) = [(e, x) | x <- (ls++rs)] ++ allPairs (right z)

testmv = left . insert 8 . insert 9 . right .right . right