module Exercises_2.idr

import Data.Vect

-- 3 & 4
{-
  I've tried a few approaches here that haven't worked. I don't agree with the
  expected behavior that `vectTake` should error when trying to take more
  elements than there are. I think it should mirror the behavior of `take`,
  which returns the whole list if there are not enough elements.
  
  I have tried finitely bounding the return Vect to the length of the input
  Vect, as well as computing it based off the implicit arguments with `minus`.
  
  I am leaving this note as a discussion point.
-}

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

vectTake' : (n : Fin (S m)) -> Vect m a -> Vect (finToNat n) a
vectTake' FZ xs = []
vectTake' (FS n') (x :: xs) = x :: vectTake' n' xs

-- 5
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just idx) => Just ((index idx xs) +
                                                   (index idx ys))
