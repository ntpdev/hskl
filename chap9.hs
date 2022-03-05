-- Chapter 9 Countdown

allpairs :: [a] -> [(a,a)]
allpairs [] = []
allpairs [x] = []
allpairs (x:xs) = [(x,y) | y <- xs] ++ allpairs xs

data Op = Add | Sub | Mul | Div deriving (Show, Eq, Ord)
data Exp = Val Int | Oper Op Exp Exp deriving (Show, Eq)

operP :: Op -> (Int, Int) -> Exp
operP op p = Oper op (Val (fst p)) (Val (snd p))

isValid :: Op -> Int -> Int -> Bool
isValid Add _ _ = True
isValid Sub x y = x /= y
isValid Div x y = let mx = max x y
                      mn = min x y
                  in mn /= 1 && mx `mod` mn == 0
isValid Mul x y = x /= 1 && y /= 1

validOps :: [(Int, Int)] -> [Exp]
validOps ps = [operP op p | p <- ps, op <- [Add, Sub, Mul, Div], isValid op (fst p) (snd p)]

eval :: Exp -> Int
eval (Val n) = n
eval (Oper op exp1 exp2) = eval2 op (eval exp1) (eval exp2)

eval2 :: Op -> Int -> Int -> Int
eval2 Add x y = x + y
eval2 Sub x y = abs (x - y)
eval2 Mul x y = x * y
eval2 Div x y = (max x y) `div` (min x y)

-- all sub sequences of a list
-- alt way [f ys |f <- [id, (x:)], ys <- yss]
subseq :: [a] -> [[a]]
subseq [] = [[]]
subseq (x:xs) = yss ++ [x : ys | ys <- yss]
                where yss = subseq xs

-- inter x (y:ys) = (x:y:ys) : map (y:) (inter x ys)
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:zs | zs <- interleave x ys]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- concat (map perms (subseq xs))
choices :: [a] -> [[a]]
choices = concat . map perms . subseq
