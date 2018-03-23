module Notes_2

import System

%default total

-- Does this need to be passed to printLn?
export
readNumber : IO (Maybe Nat)
readNumber = do 
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do n1 <- readNumber
     case n1 of
          Nothing => pure Nothing
          Just n1_ok =>
            do n2 <- readNumber
               case n2 of
                    Nothing => pure Nothing
                    Just n2_ok => pure (Just (n1_ok, n2_ok))

readNumbers' : IO (Maybe (Nat, Nat))
readNumbers' =
  do Just n1 <- readNumber | Nothing => pure Nothing
     Just n2 <- readNumber | Nothing => pure Nothing
     pure (Just (n1, n2))

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Done!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs
