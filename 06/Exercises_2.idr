module Exercises_2.idr

import Data.Vect

-- 1
Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Int)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

-- 2
-- Extended in Printf.idr

-- 3
TupleVect : Nat -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, (TupleVect k ty))

test : TupleVect 4 Nat
test = (1,2,3,4,())

