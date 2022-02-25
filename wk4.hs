-- 3.10
data Tree = Leaf | Node Int Tree Tree deriving Show

ts = Node 2 (Node 1 Leaf Leaf) (Node 4 (Node 3 Leaf Leaf) (Node 5 Leaf Leaf))

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ l r) = 1 + (max (treeDepth l) (treeDepth r))

treeSum :: Tree -> Int
treeSum Leaf         = 0
treeSum (Node x l r) = x + (treeSum l) + (treeSum r)

treeFold :: Tree -> (Int -> a -> a -> a) -> a -> a
treeFold Leaf _ x = x
treeFold (Node n l r) fn x = fn n (treeFold l fn x) (treeFold r fn x)

treeSum' ts = treeFold ts (\n l r -> n+l+r) 0
treeDepth' ts = treeFold ts (\n l r -> 1 + max l r) 0
treeDF' ts = treeFold ts (\n l r -> l ++ (n:r)) []

-- flatten tree left to right
treeDF :: Tree -> [Int]
treeDF Leaf         = []
treeDF (Node n l r) = (treeDF l) ++ (n : treeDF r)

addNewMax :: Tree -> Tree
addNewMax Leaf            = Node 0 Leaf Leaf
addNewMax (Node n l Leaf) = Node n l (Node (n+1) Leaf Leaf)
addNewMax (Node n l r)    = Node n l (addNewMax r)

grow :: Tree -> Int -> Tree
grow Leaf x = Node x Leaf Leaf
grow (Node n l Leaf) x | n < x  = Node n l (Node x Leaf Leaf)
grow (Node n l r) x    | n < x  = Node n l (grow r x)
grow (Node n Leaf r) x | n >= x = Node n (Node x Leaf Leaf) r
grow (Node n l r) x    | n >= x = Node n (grow l x) r

-- build tree
buildTree :: [Int] -> Tree
buildTree []     = Leaf
buildTree [x]    = grow Leaf x
buildTree (x:xs) = grow (buildTree xs) x

-- wk4
data CS = Score [Char] Int deriving Show

scr :: CS -> Int
scr (Score _ x) = x

winner :: CS -> CS -> CS
winner (Score a x) (Score b y)
    | x > y     = Score a x
    | otherwise = Score b y


depth :: Tree -> Int
depth Leaf = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

tsum :: Tree -> Int
tsum Leaf = 0
tsum (Node x l r) = x + (tsum l) + (tsum r)

-- given an initial range bound determine if a tree is sorted
isSorted :: Tree -> Int -> Int -> Bool
isSorted Leaf _ _ = True
isSorted (Node x l r) mn mx = x >= mn && x < mx && isSorted l mn x && isSorted r x mx

data SimpleNum = One | Two | Many Int deriving (Show, Read, Eq)

-- case expression 
simpInt :: SimpleNum -> Int
simpInt n =
    case n of
        One  -> 1
        Two  -> 2
        Many x -> x