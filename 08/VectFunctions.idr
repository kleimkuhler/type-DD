module VectFunctions

import Data.Vect

reverse' : Vect n elem -> Vect n elem
reverse' [] = []
reverse' (x :: xs) = reverseProof (reverse' xs ++ [x])
  where
    reverseProof : Vect (len + 1) elem -> Vect (S len) elem
    reverseProof {len = S len} result = rewrite plusCommutative 1 len in result

appendNil : Vect m elem -> Vect (plus m 0) elem
appendNil {m} xs = rewrite plusZeroRightNeutral m in xs

-- purpose of `sym`?
append_xs : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
append_xs {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs

append' : Vect n elem -> Vect m elem -> Vect (m + n) elem
append' [] ys = appendNil ys
append' (x :: xs) ys = append_xs (x :: append' xs ys)
