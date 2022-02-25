-- chapter 6

--4 merge
merg :: Ord a => [a] -> [a] -> [a]
merg xs [] = xs
merg [] ys = ys
merg (x:xs) (y:ys) = if x <= y
                     then x : merg xs (y:ys)
                     else y : merg (x:xs) ys

splt :: [a] -> ([a],[a])
splt xs = (take n xs, drop n xs)
          where n = (length xs) `div` 2

-- see splitAt
-- uses private fn with an accumulator
-- this reverses the first list
spltacc :: Int -> [a] -> ([a], [a])
spltacc n x = spltacc' n x ([], [])
            where spltacc' _ [] x = x
                  spltacc' 0 x (y, z) = (y,x ++ z)
                  spltacc' n (x:xs) (y, z) = spltacc' (n-1) xs (x : y, z)

splt2 :: Int -> [a] -> ([a], [a])
splt2 n ls 
  | n <= 0 = ([], ls)
  | otherwise = splt2' n ls
                where
                  splt2' _ [] = ([], [])
                  splt2' 1 (x:xs) = ([x], xs)
                  splt2' m (x:xs) = (x:ys, zs)
                    where (ys, zs) = splt2' (m-1) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = merg (msort l) (msort r)
           where (l,r) = splt (x:xs)

sum' :: Integral a => [a] -> a
sum' [] = 0
sum' (n:t) = n + sum'(t)

-- left sum using accumulator
lsum :: [Integer] -> Integer
lsum = lsumx 0
  where lsumx n [] = n
        lsumx n (h:t) = lsumx (n+h) t

take' :: Integer -> [a] -> [a]
take' n [] = []
take' 0 _ = []
take' n (h:t) = h : take' (n-1) t

last' :: [a] -> a
last' [x] = x
last' (h:t) = last t

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x -> \y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

part :: (Num a) => [a] -> [([a], [a])]
part xs = partrec xs []
  where
    partrec :: (Num a) => [a] -> [a] -> [([a], [a])]
    partrec [] ys = [([], ys)]
    partrec (x:xs) ys = (x:xs, ys) : partrec xs (ys ++ [0,x])

data Frac = Frac Integer Integer deriving (Show)

simp :: Frac -> Frac
simp (Frac n d) = if g == 1
  then Frac n d
  else Frac (div n g) (div d g)
  where g = gcd n d

plus :: Frac -> Frac -> Frac
plus (Frac a b) (Frac c d) = simp $ Frac (a*d + b*c) (b*d)

data Triple = One | Two | Three deriving (Eq, Ord, Show)

data TicTacToe a = TicTacToe {
  board :: Triple -> Triple -> a
}

data State = Empty | Nought | Cross

emptyBoard :: TicTacToe State
emptyBoard = TicTacToe $ const $ const Empty

