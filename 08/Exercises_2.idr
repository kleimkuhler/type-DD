module Exercises_2

import Data.Vect

-- 1
plusCommutative' : (n : Nat) -> (m : Nat) -> n + m = m + n
plusCommutative' Z m = rewrite plusZeroRightNeutral m in Refl
plusCommutative' (S k) m = let ind = plusCommutative' k m in
                               rewrite ind in
                                       rewrite plusSuccRightSucc m k in Refl

-- 2
reverseProof_Z : Vect n a -> Vect (plus n 0) a
reverseProof_Z {n} xs = let ind = plusZeroRightNeutral n in
                            rewrite ind in xs

-- Note: Use of `sym`
reverseProof_S : Vect (S n + len) a -> Vect (plus n (S len)) a
reverseProof_S {n} {len} xs = let ind = sym $ plusSuccRightSucc n len in
                                  rewrite ind in xs

reverse' : Vect n a -> Vect n a
reverse' xs = loop [] xs
  where
    loop : Vect n a -> Vect m a -> Vect (n + m) a
    loop acc [] = reverseProof_Z acc
    loop acc (x :: xs) = reverseProof_S $ loop (x :: acc) xs
