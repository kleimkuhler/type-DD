module Exercises

import Data.Vect

-- 1
||| Compute the length of a list.
my_length : List a -> Nat
my_length xs = length' xs 0
  where
    length' : List a -> Nat -> Nat
    length' [] k = k
    length' (x :: xs) k = length' xs (S k)

my_length' : List a -> Nat
my_length' [] = 0
my_length' (x :: xs) = 1 + my_length' xs


-- 2
||| Return the elements of a list in reverse order.
my_reverse : List a -> List a
my_reverse xs = reverse' xs []
  where
    reverse' : List a -> List a -> List a
    reverse' [] ys = ys
    reverse' (x :: xs) ys = reverse' xs (x :: ys)

my_reverse' : List a -> List a
my_reverse' [] = []
my_reverse' (x :: xs) = my_reverse' xs ++ [x]

-- 3
||| Apply a function across everything of type 'a' in a list.
my_map : (a -> b) -> List a -> List b
my_map f xs = map' xs
  where
    map' : List a -> List b
    map' [] = []
    map' (x :: xs) = f x :: map' xs

-- 4
||| Apply a function across everything of type 'a' in a Vect.
my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f xs = map' xs
  where
    map' : Vect n a -> Vect n b
    map' [] = []
    map' (x :: xs) = f x :: map' xs

-- 5
||| Transposes rows and columns of a Vect of Vects
createEmpties : Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

-- 6
||| Adds two matricies.
addMatrix : Num a => Vect m (Vect n a) -> Vect m (Vect n a) ->
            Vect m (Vect n a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = let addRest = addMatrix xs ys in
                                    zipWith (+) x y :: addRest

addMatrix' : Num a => Vect m (Vect n a) -> Vect m (Vect n a) ->
             Vect m (Vect n a)
addMatrix' = zipWith (zipWith (+))

-- 7
||| Multiplies two matricies.
multHelper : Num a => (x : Vect n a) -> (ysTrans : Vect p (Vect n a)) ->
             Vect p a
multHelper x [] = []
multHelper x (y :: ys) = let row = sum (zipWith (*) x y) in
                             row :: multHelper x ys
multMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect p a) ->
             Vect m (Vect p a)
multMatrix [] ys = []
multMatrix (x :: xs) ys = let ysTrans = transposeMat ys in
                              multHelper x ysTrans :: multMatrix xs ys

dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct xs ys = sum (zipWith (*) xs ys)

dotProductLoop : Num a => (ysTrans : Vect p (Vect n a)) -> Vect n a -> Vect p a
dotProductLoop ysTrans xs = map (dotProduct xs) ysTrans

multHelper' : Num a => (xs : Vect m (Vect n a)) -> (ysTrans : Vect p (Vect n a)) ->
              Vect m (Vect p a)
multHelper' xs ysTrans = map (dotProductLoop ysTrans) xs

multMatrix' : Num a => Vect m (Vect n a) -> Vect n (Vect p a) ->
              Vect m (Vect p a)
multMatrix' xs ys = let ysTrans = transpose ys in
                    multHelper' xs ysTrans
