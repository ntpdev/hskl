-- foldl equivalent to a loop processing from head
-- effecient if use foldl'
fldl :: (b -> a -> b) -> b -> [a] -> b
fldl f z [] = z
fldl f z (x:xs) = fldl f (f z x) xs

-- foldr constructor replacement
-- foldr can process infinte lists if f is lazy in second arg
fldr :: (a -> b -> b) -> b -> [a] -> b
fldr f z [] = z
fldr f z (x:xs) = f x (fldr f z xs)

-- fldr (:) [] = id

map2 f = fldr (\a b -> (f a) : b ) []

map3 f = fldr g []
         where g = (:) . f
