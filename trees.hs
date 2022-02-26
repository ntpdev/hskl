--
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)
data TagTree a = LeafTag | NodeTag a Int (TagTree a) (TagTree a) deriving (Show)

build :: Ord a => Tree a -> a -> Tree a
build Leaf x = Node x Leaf Leaf
build (Node v l r) x
  | x <= v = Node v (build l x) r
  | otherwise = Node v l (build r x)

builds :: Ord a => Tree a -> [a] -> Tree a
builds t [] = t
builds t (x:xs) = builds (build t x) xs

newTree :: Ord a => [a] -> Tree a
newTree = builds Leaf

mapT :: (a -> b) -> Tree a -> Tree b
mapT f Leaf = Leaf
mapT f (Node v l r) = Node (f v) (mapT f l) (mapT f r)

-- process tree from left to right
foldlT :: (b -> a -> b) -> b -> Tree a -> b
foldlT f z Leaf = z
foldlT f z (Node v l r) = foldlT f (f (foldlT f z l) v) r

flatten :: Tree a -> [a]
flatten = foldlT (\b a -> a : b) []

-- to do this tags Node with depth, change it to unique state
conv :: Int -> Tree a -> TagTree a
conv _ Leaf = LeafTag
conv n (Node v l r) = NodeTag v n (conv (n+1) l) (conv (n+1) r)