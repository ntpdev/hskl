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
eval (Oper Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Oper Sub exp1 exp2) = abs ((eval exp1) - (eval exp2))
eval (Oper Mul exp1 exp2) = (eval exp1) * (eval exp2)
eval (Oper Div exp1 exp2) = (eval exp1) `div` (eval exp2)

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

-- all ways of splitting a list so that at least 1 item on each side
splits :: [a] -> [([a],[a])]
splits [] = []
splits [_] = []
splits (x:xs) = ([x], xs) : [ (x:ps, qs) | (ps,qs) <- splits xs]

--toExp :: [Int] -> [Int] -> [Exp]
--toExp lr rs = 