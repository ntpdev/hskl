-- state monad
newtype St s a = St { runState :: s -> (s,a) }

get :: State s s

put :: s -> St s ()

modify :: (s -> s) -> St s ()