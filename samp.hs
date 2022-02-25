fib :: (Integral a) => a -> a
fib n = if n > 1
          then fib (n-1) + fib (n-2)
          else n

fibseq :: (Integral a) => a -> [a]
fibseq n = [fib x | x <- [0..n]]

fibnext :: (Integral a) => [a] -> [a]
fibnext xs = if length xs < 2
               then xs
               else (sum $ take 2 xs) : xs

-- generate a finite sequence 1 element at a time
collatz3 :: (Integral a) => a -> [a] 
collatz3 n = if n < 2
            then [n]
            else n : collatz3 (f n)
              where f x = if x `mod` 2 == 0
                          then x `div` 2
                          else x * 3 + 1

-- using guards
collatz2 :: (Integral a) => a -> [a]
collatz2 n
  | n < 2 = [n]
  | otherwise = let next x = if x `mod` 2 == 0 then x `div` 2 else x * 3 + 1
                in n : collatz2 (next n)
                                
-- generalise by separating looping from calculation of next value
coll :: Integer -> Integer
coll x = if x `mod` 2 == 0 then x `div` 2 else x * 3 + 1

-- while predicate is true apply f x to get next num in sequence
-- includes the first element that fails the test
rep :: (Integer -> Integer) -> (Integer -> Bool) -> Integer -> [Integer]
rep f p x = if p x 
              then x : rep f p (f x)
              else [x]

collatz :: Integer -> [Integer]
collatz = rep coll (>1)

-- apply f n times eg app 9 (+1) 0
app :: Integer -> (a -> a) -> a -> a
app 1 f x = f x
app n f x = f (app (n-1) f x)

-- using app 
fibgen2 :: Integer -> [Integer]
fibgen2 n = app n (\xs -> (sum $ take 2 xs) : xs) [1,0]

-- infinte lists
ones = 1 : ones
nat = 0 : map (+1) nat

-- using zip and lazy eval
fibinf = 0 : 1 : zipWith (+) fibinf (tail fibinf)

fibgen3 n = take n fibinf

-- reverse defined in terms of foldl' from Data.List
-- rev = foldl' (\xs x -> x:xs) []

choice :: Char -> [Char]
choice x = if x == '.' then "123456789" else [x]

-- cartesian product
cartprod :: [[a]] -> [[a]]
cartprod [] = [[]]
cartprod (xs:yss) = [x : ys | x <- xs, ys <- cartprod yss]

-- reduce should ensure that the value of single element lists only exist in 1 list
-- redc ["1234", "1", "34", "3"] = ["24", "1", "4", "3"]
redc :: [[Char]] -> [[Char]]
redc xs = redc2 (filter (\x -> length x == 1) xs) xs

redc2 :: [[Char]] -> [[Char]] -> [[Char]]
redc2 [] ys = ys
redc2 (x:xs) ys = redc2 xs (redc3 (head x) ys)

redc3 :: Char -> [[Char]] -> [[Char]]
redc3 c xs = map (\x -> if length x == 1 then x else filter (\y -> c /= y) x) xs

-- inter returns all possible lists formed by adding an element to a list
-- inter 'a' "bc" = ["abc","bac","bca"]
inter :: a -> [a] -> [[a]]
inter x [] = [[x]]
inter x (y:ys) = (x:y:ys) : map (y:) (inter x ys)

-- Newtons method for a fn and its derivative fn' and initial guess
approx :: (Float -> Float) -> (Float -> Float) -> Float -> Float
approx f f' x 
  | abs (1.0 - (x' / x)) < 0.01 = x'
  | otherwise = approx f f' x'
                where x' = x - (f x) / (f' x)

-- is ascending use pattern match for 2+ elem list
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = if x > y then False else isAsc (y:xs)

--edges starting from s
adj :: Eq a => a -> ([(a, b)] -> [(a,b)])
adj s = filter (\(x,_) -> x == s)
-- alternatively use a list comp
-- adj2 xs n = [(x,y) | (x,y) <- xs, x == n]

-- [(1,2),(2,3),(3,2),(4,3),(4,5)]
-- does a path exist between start -> end for the unidirectional list of edges
-- uses guards and [] <- xs which is a pattern guard
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath xs s e 
  | s == e    = True
  | [] <- xs  = False
  | otherwise = or $ map (\(s',ys) -> hasPath ys s' e) $ adj' xs s
                where adj' ys z = map (\y -> (snd y, filter (\x -> x /= y) ys)) $ filter (\(x,_) -> x == z) ys  

-- alt implementation using list comp
-- xs' calculated by removing all paths from/to x
hasPath2 :: [(Int, Int)] -> Int -> Int -> Bool
hasPath2 [] x y = x == y
hasPath2 xs x y
  | x == y = True
  | otherwise = let xs' = [(n,m) | (n,m) <- xs, n /= x && m /= x]
                in or [hasPath2 xs' m y | (n,m) <- xs, n == x]

rle :: Eq a => [a] -> [(Int, a)]
rle [] = []
rle (x:xs) = rec (1,x) xs
  where rec x [] = [x]
        rec (n,e) (x:xs) 
          | e == x    = rec (n+1,e) xs
          | otherwise = (n,e) : rec (1,x) xs

rled :: [(Int, a)] -> [a]
rled [] = []
rled ((n,x):xs) = rep x n ++ rled xs
  where rep e 0 = []
        rep e n = e : rep e (n-1)

flt :: (a -> Bool) -> [a] -> [a]
flt _ []     = []
flt p (x:xs) = if (p x) then x : flt p xs
                        else flt p xs

