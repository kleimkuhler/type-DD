module Hangman

import Data.Vect
import RemoveElem

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) -> (missing : Vect letters Char) ->
                   WordState guesses_remaining letters

data Finished : Type where
     Won : WordState (S guesses) Z -> Finished
     Lost : WordState Z (S letters) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

removeElem : (elem : a) -> (vect : Vect (S n) a) ->
             {auto prf : Elem elem vect} -> Vect n a
removeElem elem (elem :: xs) {prf = Here} = xs
removeElem {n = Z} elem (x :: []) {prf = (There later)} = absurd later
removeElem {n = (S k)} elem (x :: xs) {prf = (There later)}
           = x :: removeElem elem xs

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

processGuess : (letter : Char) -> WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters)) (WordState (S guesses) letters)
processGuess letter (MkWordState word missing)
             = case isElem letter missing of
                    Yes prf => Right (MkWordState word (removeElem letter missing))
                    No contra => Left (MkWordState word missing)

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter letter) <- readGuess
                                 case processGuess letter st of
                                       Left l => do putStrLn "Wrong!"
                                                    case guesses of
                                                          Z => pure (Lost l)
                                                          S k => game l
                                       Right r => do putStrLn "Right!"
                                                     case letters of
                                                           Z => pure (Won r)
                                                           S k => game r

main : IO ()
main = do result <- game {guesses=2}
                         (MkWordState "test" ['t', 'e', 's', 't'])
          case result of
                Won game => putStrLn "You win!"
                Lost (MkWordState word missing) =>
                     putStrLn ("You lose. The word was " ++ word)
