module Hangman

import Data.Vect

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) -> (missing : Vect letters Char) ->
                   WordState guesses_remaining letters

data Finished : Type where
     Won : WordState (S guesses) Z -> Finished
     Lost : WordState Z (S letters) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

nilNotValid : ValidInput [] -> Void
nilNotValid (Letter _) impossible

listNotValid : ValidInput (x :: (y :: xs)) -> Void
listNotValid (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No nilNotValid
isValidInput (x :: []) = Yes $ Letter x
isValidInput (x :: (y :: xs)) = No listNotValid

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput $ unpack s

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess: "
               guess <- getLine
               case isValidString guess of
                     Yes prf => pure (_ ** prf)
                     No contra => do putStrLn "Invalid guess."
                                     readGuess

game : WordState (S guesses) (S letters) -> IO Finished
game x = ?game_rhs
