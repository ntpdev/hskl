-- binary tree with nodes holding values
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)
-- tagged tree
data TagTree a = LeafTag | NodeTag a Int (TagTree a) (TagTree a) deriving (Show)

-- add one element to a tree
build :: Ord a => a -> Tree a -> Tree a
build x Leaf = Node x Leaf Leaf
build x (Node v l r)
  | x <= v = Node v (build x l) r
  | otherwise = Node v l (build x r)

builds :: Ord a => Tree a -> [a] -> Tree a
builds t [] = t
builds t (x:xs) = builds (build x t) xs

newTree2 :: Ord a => [a] -> Tree a
newTree2 = builds Leaf

-- build a tree from a list by repeatedly adding elements
newTree :: Ord a => [a] -> Tree a
newTree = foldr build Leaf 

mapT :: (a -> b) -> Tree a -> Tree b
mapT _ Leaf = Leaf
mapT f (Node v l r) = Node (f v) (mapT f l) (mapT f r)

-- process tree in sorted order from right to left i.e high to low
-- notice each expression returns b which is passed either into f or another call to foldrT
foldrT :: (a -> b -> b) -> b -> Tree a -> b
foldrT _ z Leaf = z
foldrT f z (Node v l r) = let 
                            rf = foldrT f z r
                            cf = f v rf
                          in
                            foldrT f cf l  

-- returns tree as sorted list by folding the tree
flatten :: Tree a -> [a]
flatten = foldrT (:) []

-- add unique tag to each node
conv :: Int -> Tree a -> (Int ,TagTree a)
conv n Leaf = (n, LeafTag)
conv n (Node v l r) = let
                        (n', l')  = conv n l
                        (n'', r') = conv n' r
                      in
                        (n''+1, NodeTag v n'' l' r')

