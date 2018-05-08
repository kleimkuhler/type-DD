module Exercises_2

import Data.Vect

%default total

-- 1
plusCommutative' : (n : Nat) -> (m : Nat) -> n + m = m + n
plusCommutative' Z m = sym (plusZeroRightNeutral m)
plusCommutative' (S k) m = rewrite plusCommutative' k m in
                                   plusSuccRightSucc m k

-- 2
reverseProof_Z : Vect n a -> Vect (plus n 0) a
reverseProof_Z {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_S : Vect (S n + len) a -> Vect (plus n (S len)) a
reverseProof_S {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs

reverse' : Vect n a -> Vect n a
reverse' xs = loop [] xs
  where
    loop : Vect n a -> Vect m a -> Vect (n + m) a
    loop acc [] = reverseProof_Z acc
    loop acc (x :: xs) = reverseProof_S $ loop (x :: acc) xs
