-- https://www.cis.upenn.edu/~cis194/spring13/lectures.html

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n < 0  = []
  | n < 10 = [n]
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = doe xs (toInteger n)
                      where n = 2 - (length xs `mod` 2)

doe :: [Integer] -> Integer -> [Integer]
doe [] _     = []
doe (x:xs) a = x * a : doe xs (if a == 1 then 2 else 1)

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (x `mod` 10) + (x `div` 10) + (sumDigits xs)

validate :: Integer -> Bool
validate x = let n = sumDigits $ doubleEveryOther $ toDigits x
             in n `mod` 10 == 0
