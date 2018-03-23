module Exercises_2

import System

import Notes_2

-- 1 & 3
guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses =
  do putStr "Enter guess "
     putStr (cast guesses)
     putStr ": "
     Just guessed <- readNumber
        | Nothing => do putStrLn "Invalid input"
                        guess target (S guesses)
     case compare guessed target of
       LT => do putStrLn "Too low!"
                guess target (S guesses)
       EQ => do putStrLn "Correct!"
                pure ()
       GT => do putStrLn "Too high!"
                guess target (S guesses)

-- 2
guessRandom : IO ()
guessRandom = do seed <- time
                 let random = mod seed 10
                 putStrLn "Guess a number between [0, 10)"
                 guess (cast random) (S Z)

-- 4
