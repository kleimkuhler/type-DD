module Exercises_2

import System

import Notes_2

-- 1 & 3
guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses
  = do putStr ("Enter guess " ++ (cast guesses) ++ ": ")
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
replWith' : (state : a) -> (prompt : String) ->
            (onInput : a -> String -> Maybe (String, a)) -> IO ()
replWith' state prompt onInput
  = do putStr prompt
       input <- getLine
       case onInput state input of
            Just (result, state') => do putStrLn result
                                        replWith' state' prompt onInput
            Nothing => pure ()

repl' : (prompt : String) ->
        (onInput : String -> String) -> IO ()
repl' prompt onInput
  = replWith' () prompt (\state, input => Just (onInput input, ()))
