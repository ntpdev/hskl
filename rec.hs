-- catamorphism is same a foldr
-- cataL (:) [] [a] = id [a]
-- cataL + 0 [1..3] = + 1 (+ 2 (+ 3 0))
cataL :: (a -> b -> b) -> b -> [a] -> b
cataL _ b [] = b
cataL f b (x:xs) = f x (cataL f b xs)

-- define filter in terms of fold
filterL :: (a -> Bool) -> [a] -> [a]
filterL p = cataL (\a b -> if p a then a:b else b) []

-- alternative where it provides a partially applied fn alg which takes a list
filterL' p = cataL alg []
  where alg a | p a       = (a:)
              | otherwise = id

-- anamorphism / unfoldr
-- generates an infinite list via repeated application of fn to a seed
anaL :: (t -> (a, t)) -> t -> [a]
anaL f b = let (a, b') = f b
           in a : anaL f b'

fib = anaL f (1,0)
      where f (a,b) = (b, (a+b, a))

anaL' :: (t -> Maybe (a, t)) -> t -> [a]
anaL' f b = case f b of
              Nothing      -> []
              Just (a, b') -> a : anaL' f b'

-- return finite seq ending in 1
collatz n = let 
              coll n = if n `mod` 2 == 0 then n `div` 2
                                         else 3 * n + 1
              p x = if (x < 1) then Nothing
                               else Just (x, if x > 1 then coll x else 0)
            in anaL' p n
