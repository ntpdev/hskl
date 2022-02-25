-- glasgow 2.11

allunknown :: String -> String
allunknown s = take (length s) $ repeat '*'

--       secret    display   guess -> (guessed correct, display')
check :: String -> String -> Char -> (Bool, String)
check s d g = let d' = [if x == g then x else y | (x,y) <- zip s d]
              in (elem g s, d')

turn :: String -> String -> Int -> IO ()
turn _ _ 0             = putStrLn "you lose"
turn s d n | s == d    = putStrLn "you win"
           | otherwise = mkguess s d n

mkguess :: String -> String -> Int -> IO ()
mkguess s d n =
    do putStrLn (s ++ " " ++ d ++ " " ++ (show n))
       putStr "guess: "
       q <- getLine
       let (correct, d') = check s d (q!!0)
       let n' = if correct then n else n-1
       turn s d' n'

game :: String -> Int -> IO ()
game s n = turn s (allunknown s) n

phrase :: [Char] -> [Char]
phrase s = [s!!0] ++ " is for " ++ s

combine :: [Char] -> [Char] -> [Char]
combine e a = if a == "" then e else e ++ ", " ++ a

speller :: [[Char]] -> [Char]
speller = foldr combine "" . map phrase




