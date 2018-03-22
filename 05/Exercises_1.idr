module Exercises_1

-- I did not find white space to be very intuitive here
printLonger : IO ()
printLonger = do putStr "String One: "
                 s1 <- getLine
                 putStr "String Two: "
                 s2 <- getLine
                 let l1 = length s1
                 let l2 = length s2
                 putStrLn (show (max l1 l2))

printLonger' : IO ()
printLonger' = putStr "String One: " >>= \_ =>
               getLine >>= \s1 =>
               putStr "String Two: " >>= \_ =>
               getLine >>= \s2 =>
               let l1 = length s1
                   l2 = length s2 in
                   putStrLn (show (max l1 l2))
