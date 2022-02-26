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
  | n < 2     = [n]
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

-- infinte list of primes using list comprehension
primes :: [Int]
primes = let prm (p:xs) = p : [x | x <- prm xs, x `mod` p /= 0]
         in prm [2..] 

-- same using filter
primes' :: [Int]
primes' = let prm (p:xs) = p : filter (\x -> x `mod` p /= 0) (prm xs)
          in prm [2..] 

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xxs) = xs ++ concat' xxs

rep' :: Int -> a -> [a]
rep' 0 _ = []
rep' n x = x : rep' (n-1) x

-- build n by m matrix
rep2 :: Int -> Int -> a -> [[a]]
rep2 0 _ _ = []
rep2 n m x = (rep' m x) : rep2 (n-1) m x

-- note recurse on both arguments and use underscore
idx :: [a] -> Int -> a
idx (x:_) 0 = x
idx (_:xs) n = idx xs (n-1)

-- insert value into an ordered list
ins :: Ord a => a -> [a] -> [a]
ins n [] = [n]
ins n (x:xs)
  | n <= x    = n : x : xs
  | otherwise = x : ins n xs

insort :: Ord a => [a] -> [a]
insort []     = []
insort (x:xs) = ins x (insort xs)

-- merge 2 sorted lists
mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg (x:xs) (y:ys)
  | x <= y    = x : mrg xs (y:ys)
  | otherwise = y : mrg (x:xs) ys

mrgsort' :: Ord a => [a] -> [a]
mrgsort' [] = []
mrgsort' [x] = [x]
mrgsort' xs = mrg (mrgsort' $ take n xs) (mrgsort' $ drop n xs)
              where n = length xs `div` 2

-- splits list into 2 parts
split :: Int -> [a] -> ([a], [a])
split 0 xs = ([], xs)
split _ [] = ([], [])
split n (x:xs) = let (ys, zs) = split (n-1) xs
                 in (x:ys, zs)

-- merge sort - recursively split list into two, sort each half and merge results
mrgsort :: Ord a => [a] -> [a]
mrgsort [] = []
mrgsort [x] = [x]
mrgsort xs = let (ys, zs) = split (length xs `div` 2) xs
             in mrg (mrgsort ys) (mrgsort zs)
