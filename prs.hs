import Data.Char

type Parser a = String -> [(a, String)]

pDigit :: Parser Int
pDigit = \s -> case s of
                [] -> []
                (x:xs) -> if isDigit x then [((read [x] :: Int), xs)] else []

pMany :: Parser Int -> Parser Int
pMany f = \s -> case f s of
                [] -> []
                [(v, x)] -> f x

ret :: a -> Parser a
ret a = \s -> [(a, s)]

failure :: Parser a
failure = \s -> []

item :: Parser Char
item = \s -> case s of
            [] -> []
            (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a, String)]
parse f s = f s

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \s -> case p s of
                [] -> []
                [(v,r)] -> parse (f v) r

p2Digit :: Parser Int
p2Digit = do x <- pDigit
             y <- pDigit
             ret (x*10+y)

