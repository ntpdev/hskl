--
rd :: String -> Int
rd s = read s

-- map takes 2 args so we need to eval words s before calling map
-- show sum map can be composed
fn :: String -> String
fn s = show . sum . map rd $ words s 

incr :: Int -> Int
incr n = n + 1

sq :: Int -> Int
sq n = n * n

rndup :: Int -> Int -> Int
rndup n m = if n `mod` m == 0
    then n
    else n - (n `mod` m) + m

main = interact fn